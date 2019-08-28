
/*************************************************************/
/*                                                           */
/*  Copyright (c) 1990                                       */
/*  Darrell L. Whitley                                       */
/*  Computer Science Department                              */
/*  Colorado State University                                */
/*                                                           */
/*  Permission is hereby granted to copy all or any part of  */
/*  this program for free distribution.   The author's name  */
/*  and this copyright notice must be included in any copy.  */
/*                                                           */
/*************************************************************/
extern void parse_command_line (/* int argc, char *argv[] */);
extern int  parse_config_file (/* char filename[] */);
extern void print_params(/* FILE *fp */);
extern int  set_parameter (/* char tag, char value[] */);
extern void usage();
