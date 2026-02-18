
// measured in bytes
// Note that right now if you were to call this with something which is a Type, then you'll get a bad answer.
size_when_serialized :: (value_or_type: $T) -> total_size: s64 {
    #if #run type_info(T).type == Type_Info_Tag.TYPE
        log("WARNING: you probably messed up this call, you should be passing in non-Type values.", value_or_type);

    #if T == string {
        // We store: count (s64) + the raw character bytes
        return size_of(s64) + value_or_type.count;
    }
        
    // log("size when serialized called on type %", value_or_type);
    #if #run type_info(T).type == Type_Info_Tag.STRUCT {
        // log("struct members:");
        // #insert #run for_each_member(T, "log(\"  - %1\");\n");
        // log("it is a struct");
        total_size := 0;
        #insert #run for_each_member(T, 
        "total_size += size_when_serialized(value_or_type.%1);"
        );
        return total_size;
    } 

    #if #run type_info(T).type == Type_Info_Tag.ARRAY {
        // log("it is an array");
        total_size_bytes := size_of(type_of(value_or_type.count));
        for value_or_type total_size_bytes += size_when_serialized(it);
        return total_size_bytes;
    }



    // if you pass in a Type type, this is where it would all go wrong since the size of a Type != the size of an instance of that type.

    // otherwise you're not a struct or an array this means that you're not padded in any way and you're not holding pointers to dyn mem, therefore your size is just size_of 


    // so when this is not here we get a segfault with something like this:
    // 13Segmentation fault. The faulty address is 0x0, from 0x2dc78a.
    // that is occuring from return statement expression.
    // this might be happening because jai thinks I'm not using T anywhere in the 
    // function body, but it's not checking the return statement or something
    // and so that variable gets "factored out", so this line fixes it for now
    // this is bad and I wish I understood it better, for now this works.
    t: T;

    return size_of(T);
}



serialize :: (write_ptr: *void, value: $T) -> bytes_written: int {
    // log("serialize called | write pos % | type % | size_of(T) %",  write_ptr, T, size_of(T));


    #if T == string {
        nbw := 0;

        // write the length first
        bw := serialize(write_ptr, value.count);
        write_ptr += bw;
        nbw += bw;

        // write the raw character data
        if value.count > 0 {
            memcpy(write_ptr, value.data, value.count);
            nbw += value.count;
        }
        return nbw;
    }


    #if #run type_info(T).type == Type_Info_Tag.STRUCT {
        // a struct is like a fixed size array that holds different types, so it's a sequence of types. 
        // a struct says ok put that in memory there and this in memory next to it. During serialization
        // the same theory holds and we just dump the attributes. We don't have to write the amount of data first
        // because recurively the nested calls will eventually do this. Structs are the "non-leaf" nodes in serialization.
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
        nbw += bw;
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
    info := cast(*Type_Info) T;
    // #if evalutes the boolean at compile time, checking if it's constant, all #run statements are, so this is good.


    #if T == string {
        nbr := 0;
        // read the length
        count, br := deserialize(read_ptr, s64);
        read_ptr += br;
        nbr += br;
        // allocate and copy the character data
        result: string;
        result.count = count;
        if count > 0 {
            result.data = alloc(count);
            memcpy(result.data, read_ptr, count);
            nbr += count;
        }
        return result, nbr;
    }

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
        array_info := cast(*Type_Info_Array) info;
        // extract the size and then the array

        // log("doing array");

        result: T;
        nbr: int;

        count, br := deserialize(read_ptr, int);
        read_ptr += br;
        nbr += br;


        // log("it has size: %", count);

        for 0..count-1 {
            // log("iteration %", it);
            res, nbr_inner := deserialize(read_ptr,#run get_element_type(T));

            #if #run (cast(*Type_Info_Array) type_info(T)).array_type == .FIXED {
                // this has to occur because this cannot occur on dynamic arrays right?
                // unless we reserve, but that's only for dynamic arrays so putting
                // the if here is fine, also this is a compile time if.
                result[it] = res;
            } else {
                // log("the array is dynamic, array_add");
                array_add(*result, res);
            }
            read_ptr += nbr_inner;
            nbr += nbr_inner;
        }

        // log("loop over");
        

        

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
    return size_of(u32) + size_when_serialized(value_or_type);
}

// you might want to do this so that you can read a type from a raw buffer without knowing what type it is
serialize_with_type_first :: (write_ptr: *void, value: $T) -> bytes_written: int {
    // before I would just use *T instead of *type_val, and it would crash
    // this is probably because T is just syntax and not something that existed 
    // in memory, I think...
    type_id := get_type_id(T);
    bytes_to_write := size_of(u32);
    memcpy(write_ptr, cast(*void) *type_id, bytes_to_write);
    write_ptr += bytes_to_write;

    bw := serialize(write_ptr, value);

    return bytes_to_write + bw;
}

get_type_out :: (read_ptr: *void) -> Type {
    type_id := << (cast(* u32) read_ptr);
    type, success := get_type(type_id);
    return type;
}

// NOTE: I'm not using this yet because all my types are auto bakes, and to do this
// it requires runtime looking up of the type... Also I cannot cast Any to a specific type rn?
deserialize_with_type_first :: (read_ptr: *void) -> result: Any, bytes_read: int {
    type := << (cast(* Type) read_ptr);
    // log("deserialized a type of %", type);
    read_ptr += size_of(Type);
    return deserialize(read_ptr, type);
}

