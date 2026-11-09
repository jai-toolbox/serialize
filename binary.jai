// Binary encoding is unchanged: strings and arrays have an int-sized count,
// hash tables contain a count followed by key/value pairs, and structs contain
// selected members in declaration order, without native padding.
//
// These pointer-based APIs assume a complete, trusted input buffer. They do
// not validate buffer lengths or provide a portable endian/pointer encoding.

// A bulk copy is safe only when native storage exactly matches that encoding.
// Even fixed arrays are not compatible: their encoded count is extra data.
binary_layout_matches_memory :: (info: *Type_Info) -> bool {
    if info.runtime_size < 0 return false;

    if info.type == .INTEGER || info.type == .FLOAT || info.type == .BOOL
        || info.type == .ENUM || info.type == .POINTER || info.type == .PROCEDURE {
        return true;
    }
    if info.type != .STRUCT return false;

    members := get_struct_members(info);
    defer array_free(members);
    offset: s64;
    for member: members {
        if member.offset_in_bytes != offset
            || !binary_layout_matches_memory(member.type) return false;
        offset += member.type.runtime_size;
    }
    return offset == info.runtime_size;
}

size_when_serialized :: (value: $T) -> byte_count: s64 {
    byte_count: s64;
    #if T == string {
        return size_of(int) + value.count;
    } else #if #run is_hash_table(T) {
        byte_count = size_of(int);
        for value {
            byte_count += size_when_serialized(it_index);
            byte_count += size_when_serialized(it);
        }
        return byte_count;
    } else #if #run is_array(T) {
        byte_count = size_of(int);
        #if #run binary_layout_matches_memory((cast(*Type_Info_Array) type_info(T)).element_type) {
            return byte_count + value.count * size_of(#run get_element_type(T));
        } else {
            for value byte_count += size_when_serialized(it);
            return byte_count;
        }
    } else #if #run binary_layout_matches_memory(type_info(T)) {
        // Referencing a value of T prevents Jai from factoring this
        // specialization into a constant and omitting its result storage at
        // some call sites.
        compiler_workaround: T;
        return size_of(T);
    } else #if #run is_struct(T) {
        #insert -> string {
            members := get_struct_members(T);
            defer array_free(members);
            return format_members(members,
                "byte_count += size_when_serialized(value.%1);");
        };
        return byte_count;
    } else {
        compiler_workaround: T;
        return size_of(T);
    }
}

serialize :: (write_ptr: *void, value: $T) -> bytes_written: int {
    bytes_written: int;
    #if T == string {
        header_bytes := serialize(write_ptr, value.count);
        if value.count > 0 memcpy(write_ptr + header_bytes, value.data, value.count);
        return header_bytes + value.count;
    } else #if #run is_hash_table(T) {
        bytes_written = serialize(write_ptr, value.count);
        write_ptr += bytes_written;
        for value {
            key_bytes := serialize(write_ptr, it_index);
            write_ptr += key_bytes;
            value_bytes := serialize(write_ptr, it);
            write_ptr += value_bytes;
            bytes_written += key_bytes + value_bytes;
        }
        return bytes_written;
    } else #if #run is_array(T) {
        bytes_written = serialize(write_ptr, value.count);
        write_ptr += bytes_written;
        #if #run binary_layout_matches_memory((cast(*Type_Info_Array) type_info(T)).element_type) {
            payload_bytes := value.count * size_of(#run get_element_type(T));
            if payload_bytes > 0 memcpy(write_ptr, value.data, payload_bytes);
            return bytes_written + payload_bytes;
        } else {
            for value {
                element_bytes := serialize(write_ptr, it);
                write_ptr += element_bytes;
                bytes_written += element_bytes;
            }
            return bytes_written;
        }
    } else #if #run binary_layout_matches_memory(type_info(T)) {
        memcpy(write_ptr, *value, size_of(T));
        return size_of(T);
    } else #if #run is_struct(T) {
        member_bytes: int;
        #insert -> string {
            members := get_struct_members(T);
            defer array_free(members);
            return format_members(members,
                "member_bytes = serialize(write_ptr, value.%1); write_ptr += member_bytes; bytes_written += member_bytes;");
        };
        return bytes_written;
    } else {
        memcpy(write_ptr, *value, size_of(T));
        return size_of(T);
    }
}

get_element_type :: ($T: Type) -> Type {
    array: T;
    return array_element_type(array);
}

array_element_type :: (array: [] $Element) -> Type { return Element; }

// Callback cadence remains one notification before every 1024 array elements.
// Copying in those same chunks keeps progress callbacks useful on large arrays.
copy_deserialized_array :: (
    destination: *void,
    source: *void,
    count: int,
    element_size: int,
    progress_callback: (user_data: *void),
    progress_context: *void,
) {
    if progress_callback == null {
        if count > 0 && element_size > 0 memcpy(destination, source, count * element_size);
        return;
    }

    offset := 0;
    while offset < count {
        progress_callback(progress_context);
        chunk_count := min(count - offset, 1024);
        chunk_bytes := chunk_count * element_size;
        if chunk_bytes > 0 memcpy(destination, source, chunk_bytes);
        destination += chunk_bytes;
        source += chunk_bytes;
        offset += chunk_count;
    }
}

deserialize :: (
    read_ptr: *void,
    $T: Type,
    progress_callback: (user_data: *void) = null,
    progress_context: *void = null,
) -> result: T, bytes_read: int {
    result: T;
    bytes_read: int;
    #if T == string {
        count, header_bytes := deserialize(read_ptr, int);
        assert(count >= 0);
        result.count = count;
        if count > 0 {
            result.data = alloc(count);
            memcpy(result.data, read_ptr + header_bytes, count);
        }
        return result, header_bytes + count;
    } else #if #run is_hash_table(T) {
        count, header_bytes := deserialize(read_ptr, int);
        assert(count >= 0);
        read_ptr += header_bytes;
        bytes_read = header_bytes;
        for index: 0..count-1 {
            if progress_callback != null && (index & 1023) == 0 progress_callback(progress_context);
            key, key_bytes := deserialize(read_ptr, T.Key_Type, progress_callback, progress_context);
            read_ptr += key_bytes;
            value, value_bytes := deserialize(read_ptr, T.Value_Type, progress_callback, progress_context);
            read_ptr += value_bytes;
            table_add(*result, key, value);
            bytes_read += key_bytes + value_bytes;
        }
        return result, bytes_read;
    } else #if #run is_array(T) {
        count, header_bytes := deserialize(read_ptr, int);
        assert(count >= 0);
        read_ptr += header_bytes;
        bytes_read = header_bytes;
        #if #run (cast(*Type_Info_Array) type_info(T)).array_type == .FIXED {
            assert(count <= result.count);
        } else {
            // Both resizable arrays and views own their deserialized storage.
            // Every element is overwritten below, so skip default initialization.
            array_resize(*result, count, initialize = false);
        }

        #if #run binary_layout_matches_memory((cast(*Type_Info_Array) type_info(T)).element_type) {
            element_size := size_of(#run get_element_type(T));
            copy_deserialized_array(result.data, read_ptr, count, element_size,
                progress_callback, progress_context);
            return result, bytes_read + count * element_size;
        } else {
            for index: 0..count-1 {
                if progress_callback != null && (index & 1023) == 0 progress_callback(progress_context);
                element, element_bytes := deserialize(read_ptr, #run get_element_type(T),
                    progress_callback, progress_context);
                result[index] = element;
                read_ptr += element_bytes;
                bytes_read += element_bytes;
            }
            return result, bytes_read;
        }
    } else #if #run binary_layout_matches_memory(type_info(T)) {
        // A copy also handles unaligned fields in the tightly packed wire data.
        memcpy(*result, read_ptr, size_of(T));
        return result, size_of(T);
    } else #if #run is_struct(T) {
        member_bytes: int;
        #insert -> string {
            members := get_struct_members(T);
            defer array_free(members);
            return format_members(members,
                "result.%1, member_bytes = deserialize(read_ptr, type_of(T.%1), progress_callback, progress_context); read_ptr += member_bytes; bytes_read += member_bytes;");
        };
        return result, bytes_read;
    } else {
        memcpy(*result, read_ptr, size_of(T));
        return result, size_of(T);
    }
}

deserialize_with_type_first :: (
    read_ptr: *void,
    $T: Type,
    allocator := context.allocator,
) -> result: T, bytes_read: int {
    push_allocator(allocator);
    result, payload_bytes := deserialize(read_ptr + size_of(u32), T);
    return result, size_of(u32) + payload_bytes;
}

size_when_serialized_with_type_first :: (value: $T) -> s64 {
    return size_of(u32) + size_when_serialized(value);
}

serialize_with_type_first :: (value: $T) -> data: *void, bytes_written: int {
    data := alloc(size_when_serialized_with_type_first(value));
    bytes_written := serialize_with_type_first(data, value);
    return data, bytes_written;
}

serialize_with_type_first :: (write_ptr: *void, value: $T) -> bytes_written: int {
    type_id, _ := get_type_id(T);
    header_bytes := serialize(write_ptr, type_id);
    payload_bytes := serialize(write_ptr + header_bytes, value);
    return header_bytes + payload_bytes;
}

get_type_out :: (read_ptr: *void) -> Type {
    type_id, _ := deserialize(read_ptr, u32);
    registered_type, _ := get_type(type_id);
    return registered_type;
}
