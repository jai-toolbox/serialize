#import "Basic";
#import "Hash_Table";
#import "Hash";

/*

we don't handle things like 

Apollo_Time :: #type,isa S128;

yet, the key to doing this is resolving it to the actual underlying type somehow lookin in Preload.jai for more details when this matters more.

*/

My_Type_Info :: struct  {
    type : Type;
    name : string;
    type_id : u32;
};

types : [..] My_Type_Info;

deinit_types :: () {
    for types {
        free(it.name);
    }
    array_free(types);
}

add_type :: (types : *[..] My_Type_Info, type: Type) {
    type_name := sprint("%", type);
    mti := My_Type_Info.{type, type_name, get_hash(type_name)};
    array_add(types, mti);
}

get_type :: (type_id: u32) -> Type, bool {
    for types  {
        if it.type_id == type_id 
            return it.type, true;
    }

    return Any, false;
}

get_type_id :: (type: Type) -> u32, bool {
    for types  {
        type_name := tprint("%", type);
        if type_name == it.name
            return it.type_id, true;
    }
    return 0, false;
}

Filter_Mode :: enum {
    ONLY;
    EXCEPT;
}

// Returns an owned array of reflected members; names and type metadata are
// borrowed from the compiler. The caller releases only the returned array.
// Accepts either a Type or its reflection pointer.
get_struct_members :: (
    struct_type: $T,
    filter_names: [] string = .[],
    filter_mode: Filter_Mode = .EXCEPT,
) -> members: [..] Type_Info_Struct_Member {
    members: [..] Type_Info_Struct_Member;
    struct_info := cast(*Type_Info_Struct) struct_type;
    assert(struct_info.type == .STRUCT);

    for member, member_index: struct_info.members {
        if member.flags & .CONSTANT continue;
        
        already_covered := false;
        for prev_index: 0..member_index-1 {
            // this handles the situation where you have an overlay member.
            if struct_info.members[prev_index].offset_in_bytes == member.offset_in_bytes {
                already_covered = true;
                break;
            }
        }
        if already_covered continue;
        
        if filter_names.count > 0 {
            name_in_list := false;
            for filter_name: filter_names {
                if member.name == filter_name {
                    name_in_list = true;
                    break;
                }
            }
            
            if filter_mode == .ONLY && !name_in_list continue;
            if filter_mode == .EXCEPT && name_in_list continue;
        }
        
        array_add(*members, member);
    }

    return members;
}

// Formats and concatenates an already selected list. This does not filter
// members or take ownership of their array. %1 is the member name.
format_members :: (
    members: [] Type_Info_Struct_Member,
    format: string,
    last_format: string = "",
) -> string {
    builder: String_Builder;
    defer free_buffers(*builder);

    for member, index: members {
        if index == members.count - 1 && last_format.count > 0 {
            print_to_builder(*builder, last_format, member.name);
        } else {
            print_to_builder(*builder, format, member.name);
        }
    }

    return builder_to_string(*builder);
}

#load "binary.jai";
#load "string.jai";
