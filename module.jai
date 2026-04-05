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
    struct_name : string;
    type_id : u32;
};

types : [..] My_Type_Info;

add_type :: (types : *[..] My_Type_Info, type: Type) {
    ti := cast(*Type_Info_Struct) type;
    struct_name :string = ti.name; 
    mti := My_Type_Info.{type, struct_name, get_hash(struct_name)};
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
        struct_name := (<< cast(*Type_Info_Struct) type).name;
        if struct_name == it.struct_name
            return it.type_id, true;
    }
    return 0, false;
}

Filter_Mode :: enum {
    ONLY;
    EXCEPT;
}

for_each_member :: ($T: Type, format: string, $filter_names: [] string = .[], $filter_mode: Filter_Mode = .EXCEPT, last_format: string = "") -> string {
    builder: String_Builder;
    defer free_buffers(*builder);

    struct_info := cast(*Type_Info_Struct) T;
    assert(struct_info.type == Type_Info_Tag.STRUCT);

    // First pass: collect the indices of members we'll actually emit
    filtered_members: [..] int;
    defer array_free(filtered_members);

    for member, member_index: struct_info.members {
        if member.flags & .CONSTANT continue;
        
        already_covered := false;
        for prev_index: 0..member_index-1 {
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
        
        array_add(*filtered_members, member_index);
    }

    // Second pass: emit, using last_format for the final member if provided
    for i: 0..filtered_members.count-1 {
        member_index := filtered_members[i];
        member := struct_info.members[member_index];
        
        is_last := (i == filtered_members.count - 1);
        
        if is_last && last_format.count > 0 {
            print_to_builder(*builder, last_format, member.name);
        } else {
            print_to_builder(*builder, format, member.name);
        }
    }

    return builder_to_string(*builder);
}

#load "binary.jai";
#load "string.jai";
