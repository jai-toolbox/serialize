Print_Options :: struct {
    pretty: bool = false;
    indent_level: int = 0;
    indent_string: string = "    ";
}

struct_to_string_pretty :: (s: $T, $filter_names: [] string = .[], $filter_mode: Filter_Mode = .EXCEPT, indent_string: string = "    ") -> string, success: bool {
    result, success := struct_to_string(s, filter_names, filter_mode, .{pretty = true, indent_string = indent_string});
    return result, success;
}

to_string :: (to_serialize: $T, builder: *String_Builder, $filter_names: []string = .[], $filter_mode: Filter_Mode = .EXCEPT, options: Print_Options = .{}) -> success: bool {
    #if #run is_struct(T) {
        result, success := struct_to_string(to_serialize, filter_names, filter_mode, options);
        if !success return false;
        print_to_builder(builder, "%", result);
    } else #if #run is_array(T) {
        if to_serialize.count == 0 {
            print_to_builder(builder, ".[]");
        } else if options.pretty {
            print_to_builder(builder, ".[\n");
            nested_options := options;
            nested_options.indent_level += 1;
            for to_serialize {
                print_indent(builder, nested_options);
                if !to_string(it, builder, filter_names, filter_mode, nested_options) return false;
                if it_index < to_serialize.count - 1 {
                    print_to_builder(builder, ",\n");
                } else {
                    print_to_builder(builder, "\n");
                }
            }
            print_indent(builder, options);
            print_to_builder(builder, "]");
        } else {
            print_to_builder(builder, ".[");
            for to_serialize {
                if !to_string(it, builder, filter_names, filter_mode, options) return false;
                if it_index < to_serialize.count - 1 print_to_builder(builder, ", ");
            }
            print_to_builder(builder, "]");
        }
    } else {
        print_to_builder(builder, "%", to_serialize);
    }
    return true;
}

struct_to_string :: (s: $T, $filter_names: []string = .[], $filter_mode: Filter_Mode = .EXCEPT, options: Print_Options = .{}) -> string, success: bool {
    builder: String_Builder;
    defer free_buffers(*builder);

    struct_info := cast(*Type_Info_Struct) T;
    struct_name := struct_info.name;
    
    if options.pretty {
        print_to_builder(*builder, "%.{\n", struct_name);
        nested_options := options;
        nested_options.indent_level += 1;
        #insert #run for_each_member(T, 
            "print_indent(*builder, nested_options); print_to_builder(*builder, \"%1 = \"); if !to_string(s.%1, *builder, filter_names, filter_mode, nested_options) return \"\", false; print_to_builder(*builder, \",\\n\");\n", 
            filter_names, filter_mode,
            last_format = "print_indent(*builder, nested_options); print_to_builder(*builder, \"%1 = \"); if !to_string(s.%1, *builder, filter_names, filter_mode, nested_options) return \"\", false; print_to_builder(*builder, \"\\n\");\n"
        );
        print_indent(*builder, options);
        print_to_builder(*builder, "}");
    } else {
        print_to_builder(*builder, "%.{", struct_name);
        #insert #run for_each_member(T, 
            "print_to_builder(*builder, \"%1 = \"); if !to_string(s.%1, *builder, filter_names, filter_mode, options) return \"\", false; print_to_builder(*builder, \", \");\n", 
            filter_names, filter_mode,
            last_format = "print_to_builder(*builder, \"%1 = \"); if !to_string(s.%1, *builder, filter_names, filter_mode, options) return \"\", false;\n"
        );
        print_to_builder(*builder, "}");
    }
    
    return builder_to_string(*builder), true;
}

print_indent :: (builder: *String_Builder, options: Print_Options) {
    for 0..options.indent_level-1 {
        print_to_builder(builder, "%", options.indent_string);
    }
}

is_struct :: ($T: Type) -> bool {
    info := cast(*Type_Info) T;
    return info.type == .STRUCT;
}

is_array :: ($T: Type) -> bool {
    info := cast(*Type_Info) T;
    return info.type == .ARRAY;
}

parse_whitespace :: (s: string, pos: int) -> int {
    p := pos;
    while p < s.count && (s[p] == #char " " || s[p] == #char "\n" || s[p] == #char "\r" || s[p] == #char "\t") {
        p += 1;
    }
    return p;
}

parse_expect :: (s: string, pos: int, expected: u8) -> int, bool {
    p := parse_whitespace(s, pos);
    if p >= s.count || s[p] != expected return p, false;
    return p + 1, true;
}

parse_identifier :: (s: string, pos: int) -> string, int {
    p := parse_whitespace(s, pos);
    start := p;
    while p < s.count && (s[p] != #char " " && s[p] != #char "=" && s[p] != #char "." && s[p] != #char "{" && s[p] != #char "}" && s[p] != #char "," && s[p] != #char "[" && s[p] != #char "]" && s[p] != #char "\n" && s[p] != #char "\r" && s[p] != #char "\t") {
        p += 1;
    }
    result: string;
    result.data = s.data + start;
    result.count = p - start;
    return result, p;
}

parse_value_token :: (s: string, pos: int) -> string, int {
    p := parse_whitespace(s, pos);
    start := p;
    while p < s.count && s[p] != #char "," && s[p] != #char "}" && s[p] != #char "]" && s[p] != #char "\n" && s[p] != #char "\r" {
        p += 1;
    }
    end := p;
    while end > start && (s[end-1] == #char " " || s[end-1] == #char "\t") {
        end -= 1;
    }
    result: string;
    result.data = s.data + start;
    result.count = end - start;
    return result, p;
}

from_string :: (s: string, $T: Type) -> result: T, success: bool, bytes_read: int {
    result: T;
    pos := 0;
    ok: bool;

    #if T == string {
        token, new_pos := parse_value_token(s, pos);
        result = copy_string(token);
        return result, true, new_pos;
    }

    #if T == int || T == s8 || T == s16 || T == s32 || T == s64 {
        token, new_pos := parse_value_token(s, pos);
        val, ok_int, _ := string_to_int(token, 10, T);
        if !ok_int return result, false, pos;
        return val, true, new_pos;
    }

    #if T == u8 || T == u16 || T == u32 || T == u64 {
        token, new_pos := parse_value_token(s, pos);
        val, ok_int, _ := string_to_int(token, 10, T);
        if !ok_int return result, false, pos;
        return val, true, new_pos;
    }

    #if T == float32 || T == float64 {
        token, new_pos := parse_value_token(s, pos);
        val, ok_float, _ := string_to_float(token);
        if !ok_float return result, false, pos;
        result = cast(T) val;
        return result, true, new_pos;
    }

    #if T == bool {
        token, new_pos := parse_value_token(s, pos);
        if token == "true" return true, true, new_pos;
        if token == "false" return false, true, new_pos;
        return false, false, pos;
    }

    #if #run is_struct(T) {
        _, new_pos := parse_identifier(s, pos);
        pos = new_pos;

        pos, ok = parse_expect(s, pos, #char ".");
        if !ok return result, false, pos;

        pos, ok = parse_expect(s, pos, #char "{");
        if !ok return result, false, pos;

        while true {
            pos = parse_whitespace(s, pos);
            if pos >= s.count return result, false, pos;
            if s[pos] == #char "}" { pos += 1; break; }

            if s[pos] == #char "," { pos += 1; continue; }

            member_name: string;
            member_name, pos = parse_identifier(s, pos);

            pos, ok = parse_expect(s, pos, #char "=");
            if !ok return result, false, pos;

            pos = parse_whitespace(s, pos);

            parsed_any := false;
            #insert #run for_each_member(T, #string DONE
                if member_name == "%1" {
                    remaining: string;
                    remaining.data = s.data + pos;
                    remaining.count = s.count - pos;
                    val, inner_ok, br := from_string(remaining, type_of(T.%1));
                    if !inner_ok return result, false, pos;
                    result.%1 = val;
                    pos += br;
                    parsed_any = true;
                }
DONE
            );

            if !parsed_any return result, false, pos;
        }

        return result, true, pos;
    }

    #if #run is_array(T) {
        pos, ok = parse_expect(s, pos, #char ".");
        if !ok return result, false, pos;

        pos, ok = parse_expect(s, pos, #char "[");
        if !ok return result, false, pos;

        while true {
            pos = parse_whitespace(s, pos);
            if pos >= s.count return result, false, pos;
            if s[pos] == #char "]" { pos += 1; break; }
            if s[pos] == #char "," { pos += 1; continue; }

            remaining: string;
            remaining.data = s.data + pos;
            remaining.count = s.count - pos;

            val, inner_ok, br := from_string(remaining, #run get_element_type(T));
            if !inner_ok return result, false, pos;
            array_add(*result, val);
            pos += br;
        }

        return result, true, pos;
    }

    return result, false, pos;
}
