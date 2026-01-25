#import "Basic";

// this function allows us to make iterating over a type a compile time operation by using it with #run
for_each_member :: ($T: Type, format: string) -> string {
    builder: String_Builder;
    defer free_buffers(*builder);

    struct_info := cast(*Type_Info_Struct) T;
    assert(struct_info.type == Type_Info_Tag.STRUCT);

    for struct_info.members 
    {
        if it.flags & .CONSTANT continue;
        print_to_builder(*builder, format, it.name);
    }

    return builder_to_string(*builder);
}

// measured in bytes
// Note that right now if you were to call this with something which is a Type, then you'll get a bad answer.
size_when_serialized :: (value_or_type: $T) -> total_size: s64 {
    #if #run type_info(T).type == Type_Info_Tag.TYPE
        log("WARNING: you probably messed up this call, you should be passing in non-Type values.", value_or_type);
        
    // log("size when serialized called on type %", value_or_type);
    #if #run type_info(T).type == Type_Info_Tag.STRUCT {
        // log("it is a struct");
        total_size := 0;
        #insert #run for_each_member(T, 
        "total_size += size_when_serialized(value_or_type.%1);"
        );
        return total_size;
    } else {
        // log("it is not a struct");
        // if you pass in a Type type, this is where it would all go wrong since the size of a Type != the size of an instance of that type.
        return size_of(T);
    }
}




serialize :: (write_ptr: *void, value: $T) -> bytes_written: int {
    // log("serialize called | write pos % | type % | size_of(T) %",  write_ptr, T, size_of(T));

    #if #run type_info(T).type == Type_Info_Tag.STRUCT {
        nbw, bw: int;
        // log("serializing struct of type: %", T);
        #insert #run for_each_member(T, 
            "bw = serialize(write_ptr, value.%1); if bw != 0 { write_ptr += bw; nbw += bw; }"
        );
        return nbw;
    } 

    #if #run type_info(T).type == Type_Info_Tag.ARRAY {
        nbw, bw: int;
        // first we need to write the number of elements so that we know when to stop when deserializing
        bw = serialize(write_ptr, value.count);
        write_ptr += bw;
        for value {
            bw = serialize(write_ptr, it);
            write_ptr += bw;
            nbw += bw;
        }
        return nbw;
    }

    // log("serializing non-struct type % with value %, going to write % bytes", T, value, size_of(T));
    bytes_to_write := size_of(T);
    memcpy(write_ptr, *value, bytes_to_write);
    // log("returning");
    return bytes_to_write;
}

element_type_extractor :: (array: [] $T) -> Type {
    return T;
}

get_element_type :: ($T: Type) -> Type {
    t : T;
    return element_type_extractor(t);
}

deserialize :: (read_ptr: *void, $T: Type) -> result: T, bytes_read: int {

    // log("deserialize called | read pos % | type % | size_of(T) %",  read_ptr, T, size_of(T));
    // #if evalutes the boolean at compile time, checking if it's constant, all #run statements are, so this is good.
    #if #run type_info(T).type == Type_Info_Tag.STRUCT {
        // log("got struct of type: %", T);
        // when you have a struct you never have to update position, think about it
        result: T;
        nbr, br: int;
        #insert #run for_each_member(T, 
        "result.%1, br = deserialize(read_ptr, type_of(T.%1)); read_ptr += br; nbr += br;"
        );
        return result, nbr;
    }

    #if #run type_info(T).type == Type_Info_Tag.ARRAY {
        // extract the size and then the array

        result: T;
        nbr: int;

        count, br := deserialize(read_ptr, int);
        read_ptr += br;
        nbr += br;

        for 0..count-1 {
            res, nbr_inner := deserialize(read_ptr,#run get_element_type(T));
            result[it] = res;
            read_ptr += nbr_inner;
            nbr += nbr_inner;
        }
        

        return result, nbr;
    }

    // log("got non struct of type %", T);
    // here we read the value out. Note this will not work for dynmic storage types later.
    result_ := << (cast(* T) read_ptr);
    bytes_read := size_of(T); // probably wrong in future.
    // log("found a value of %", result);
    return result_, bytes_read;
}


size_when_serialized_with_type_first :: (value_or_type: $T) -> total_size: s64 {
    return size_of(Type) + size_when_serialized(value_or_type);
}

// you might want to do this so that you can read a type from a raw buffer without knowing what type it is
serialize_with_type_first :: (write_ptr: *void, value: $T) -> bytes_written: int {
    // before I would just use *T instead of *type_val, and it would crash
    // this is probably because T is just syntax and not something that existed 
    // in memory, I think...
    type_val : Type = T;
    bytes_to_write := size_of(Type);
    memcpy(write_ptr, cast(*void) *type_val, bytes_to_write);
    write_ptr += bytes_to_write;

    bw := serialize(write_ptr, value);

    return bytes_to_write + bw;
}

get_type_out :: (read_ptr: *void) -> Type {
    type := << (cast(* Type) read_ptr);
    return type;
}

// NOTE: I'm not using this yet because all my types are auto bakes, and to do this
// it requires runtime looking up of the type... Also I cannot cast Any to a specific type rn?
deserialize_with_type_first :: (read_ptr: *void) -> result: Any, bytes_read: int {
    type := << (cast(* Type) read_ptr);
    log("deserialized a type of %", type);
    read_ptr += size_of(Type);
    return deserialize(read_ptr, type);
}

// builds the string into the builder
structure_to_string :: (s: $T, builder: *String_Builder) -> success: bool {
    #insert #run for_each_member(T, "if !to_string(s.%1, builder) return false;\n" );  // (1)
    return true;
}

to_string  :: (to_serialize: $T, builder: *String_Builder) -> success: bool { 
    print_to_builder(builder, "%-", to_serialize);
    return true; 
}
