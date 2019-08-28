//
//      TOLKIEN: Created by Anthony Tang Yiu Cheung
//
//      Copyright (C) 1992 Chinese University of Hong Kong,
//      All rights reserved.
//
typedef void (*one_arg_error_handler_t)(const char*);
typedef void (*two_arg_error_handler_t)(const char*, const char*);
extern two_arg_error_handler_t lib_error_handler;

extern two_arg_error_handler_t
       set_lib_error_handler(two_arg_error_handler_t f);

