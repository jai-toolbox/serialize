// From this module's directory: jai tests.jai && ./.build/serialize_tests
#load "module.jai";
#import "Compiler";

#run {
    set_build_options_dc(.{
        output_path = ".build/",
        output_executable_name = "serialize_tests",
    });
}

Test_Vector :: struct { x, y, z: float32; }
Test_Empty :: struct {}
Test_Constants :: struct { value: u32; LIMIT :: 99; other: u32; }
Test_Using :: struct { using base: Test_Vector; }
Test_Padded :: struct { tag: u8; value: u64; tail: u16; }
Test_Overlay :: struct {
    x, y, z: float32;
    #overlay (x) xyz: Test_Vector;
}
Test_Fixed_Member :: struct { values: [2] u16; }
Test_Dynamic :: struct { name: string; values: [..] Test_Vector; }
Test_Enum :: enum u16 { FIRST; SECOND; }

test_progress :: (data: *void) { (cast(*int) data).* += 1; }

// Independent slow array encoding: a count followed by each encoded element.
// Comparing against this catches accidentally copying struct padding or
// forgetting a nested fixed array's count header.
test_array :: (value: $T, expected_notifications: int = -1) {
    size := size_when_serialized(value);
    bytes := alloc(size);
    expected := alloc(size);
    defer free(bytes);
    defer free(expected);
    assert(serialize(bytes, value) == size);
    offset := serialize(expected, value.count);
    for value offset += serialize(expected + offset, it);
    assert(offset == size && memcmp(bytes, expected, size) == 0);

    decoded, read := deserialize(bytes, T);
    notifications: int;
    with_progress, progress_read := deserialize(bytes, T, test_progress, *notifications);
    #if #run (cast(*Type_Info_Array) type_info(T)).array_type != .FIXED {
        defer array_free(decoded);
        defer array_free(with_progress);
    }
    assert(read == size && progress_read == size);
    expected_count := ifx expected_notifications >= 0 then expected_notifications
        else (value.count + 1023) / 1024;
    assert(notifications == expected_count);
    assert(serialize(expected, decoded) == size && memcmp(bytes, expected, size) == 0);
    assert(serialize(expected, with_progress) == size && memcmp(bytes, expected, size) == 0);
}

test_members :: () {
    members := get_struct_members(Test_Overlay);
    defer array_free(members);
    assert(members.count == 3);
    assert(members[0].name == "x" && members[1].name == "y" && members[2].name == "z");
    generated := format_members(members, "%1,", "%1");
    defer free(generated);
    assert(generated == "x,y,z");

    only := get_struct_members(Test_Overlay, string.["z", "x"], .ONLY);
    except := get_struct_members(Test_Overlay, string.["y"], .EXCEPT);
    missing := get_struct_members(Test_Overlay, string.["missing"], .ONLY);
    defer array_free(only);
    defer array_free(except);
    defer array_free(missing);
    assert(only.count == 2 && only[0].name == "x" && only[1].name == "z");
    assert(except.count == 2 && except[0].name == "x" && except[1].name == "z");
    assert(missing.count == 0);
    empty := format_members(missing, "%1,");
    defer free(empty);
    assert(empty.count == 0);

    constants := get_struct_members(Test_Constants);
    defer array_free(constants);
    assert(constants.count == 2 && constants[0].name == "value" && constants[1].name == "other");

    assert(binary_layout_matches_memory(type_info(Test_Vector)));
    assert(binary_layout_matches_memory(type_info(Test_Overlay)));
    assert(!binary_layout_matches_memory(type_info(Test_Padded)));
    assert(!binary_layout_matches_memory(type_info(Test_Fixed_Member)));
    assert(!binary_layout_matches_memory(type_info(Test_Dynamic)));
}

test_nested :: () {
    value: Test_Dynamic;
    value.name = "nested";
    array_add(*value.values, .{1, 2, 3});
    array_add(*value.values, .{4, 5, 6});
    defer array_free(value.values);
    size := size_when_serialized(value);
    bytes := alloc(size);
    expected := alloc(size);
    defer free(bytes);
    defer free(expected);
    assert(serialize(bytes, value) == size);
    offset := serialize(expected, value.name);
    offset += serialize(expected + offset, value.values);
    assert(offset == size && memcmp(bytes, expected, size) == 0);
    decoded, read := deserialize(bytes, Test_Dynamic);
    defer free(decoded.name);
    defer array_free(decoded.values);
    assert(read == size && decoded.name == value.name && decoded.values.count == 2);
    assert(decoded.values[1].z == 6);

    names := string.["", "one", "two"];
    names_size := size_when_serialized(names);
    names_bytes := alloc(names_size);
    defer free(names_bytes);
    assert(serialize(names_bytes, names) == names_size);
    decoded_names, names_read := deserialize(names_bytes, type_of(names));
    defer { for decoded_names if it.count > 0 free(it); }
    assert(names_read == names_size);
    for names assert(it == decoded_names[it_index]);

    rows: [..] [..] u8;
    row: [..] u8;
    array_add(*row, 3);
    array_add(*row, 4);
    array_add(*rows, row);
    array_add(*rows);
    defer { for rows array_free(it); array_free(rows); }
    rows_size := size_when_serialized(rows);
    rows_bytes := alloc(rows_size);
    defer free(rows_bytes);
    assert(serialize(rows_bytes, rows) == rows_size);
    notifications: int;
    decoded_rows, rows_read := deserialize(rows_bytes, type_of(rows), test_progress, *notifications);
    defer { for decoded_rows array_free(it); array_free(decoded_rows); }
    assert(rows_read == rows_size && notifications == 2);
    assert(decoded_rows.count == 2 && decoded_rows[0][1] == 4 && decoded_rows[1].count == 0);
}

test_hash_table :: () {
    table: Table(u32, u64);
    defer deinit(*table);
    table_add(*table, 7, 11);
    table_add(*table, 13, 17);
    size := size_when_serialized(table);
    bytes := alloc(size);
    defer free(bytes);
    assert(serialize(bytes, table) == size);
    notifications: int;
    decoded, read := deserialize(bytes, type_of(table), test_progress, *notifications);
    defer deinit(*decoded);
    assert(read == size && decoded.count == 2 && notifications == 1);
    found, value := table_find(*decoded, 13);
    assert(found && value == 17);
}

test_text :: () {
    value := Test_Vector.{1, 2, 3};
    text, ok := struct_to_string(value, string.["y"], .EXCEPT);
    defer free(text);
    assert(ok && text == "Test_Vector.{x = 1, z = 3}");
    pretty, pretty_ok := struct_to_string_pretty(value);
    defer free(pretty);
    parsed, parse_ok, read := from_string(pretty, Test_Vector);
    assert(pretty_ok && parse_ok && read == pretty.count);
    assert(parsed.x == 1 && parsed.y == 2 && parsed.z == 3);
}

test_type_headers :: () {
    add_type(*types, Test_Vector);
    defer deinit_types();
    value := Test_Vector.{1, 2, 3};
    bytes, written := serialize_with_type_first(value);
    defer free(bytes);
    assert(written == size_when_serialized_with_type_first(value));
    assert(get_type_out(bytes) == Test_Vector);
    decoded, read := deserialize_with_type_first(bytes, Test_Vector);
    assert(read == written && decoded.x == 1 && decoded.z == 3);
}

main :: () {
    test_members();
    test_array(u8.[0, 1, 127, 128, 255]);
    test_array(u16.[0, 1, 65535]);
    test_array(s32.[-1, 0, 123456]);
    test_array(float32.[-1, 0, 1.5]);
    test_array(bool.[false, true]);
    test_array(Test_Enum.[.FIRST, .SECOND]);
    test_array(Test_Vector.[.{1, 2, 3}, .{4, 5, 6}]);
    test_array(Test_Overlay.[.{1, 2, 3}]);
    test_array(Test_Empty.[.{}]);
    test_array(Test_Constants.[.{1, 2}]);
    test_array(Test_Using.[.{.{1, 2, 3}}]);
    test_array(Test_Padded.[.{1, 2, 3}, .{4, 5, 6}]);
    test_array(Test_Fixed_Member.[.{u16.[1, 2]}, .{u16.[3, 4]}], 3);
    bytes: [..] u8;
    defer array_free(bytes);
    test_array(bytes);
    for count: int.[1, 1023, 1024, 1025, 4097] {
        array_resize(*bytes, count);
        for *bytes it.* = cast,no_check(u8) it_index;
        test_array(bytes);
    }
    view: [] u8 = bytes;
    test_array(view);
    test_nested();
    test_hash_table();
    test_text();
    test_type_headers();
    print("Serialize tests passed\n");
}
