/*
 *	LICE - LInear Cellular Evolution strategy
 *
 *	Copyright (C) 1994 Joachim Sprave
 *
 *	This program is free software; you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation; either version 2 of the License, or
 *	(at your option) any later version.
 *
 *	This program is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with this program; if not, write to the Free Software
 *	Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */


/*
 *	See README for details of LICE.
 *	Send bugs (better: bug desciptions) and comments to
 *
 * /---------------------------------------------------------------------\
 * | Joachim Sprave                  joe@ls11.informatik.uni-dortmund.de |
 * |                      //////\\                                       |
 * | Univ. Dortmund      /        \        44221 Dortmund                |
 * | Dept. CS           _|  _   _ |_       Tel.: +49-231-755 4678        |
 * | Systems Analysis  |.|-(.)-(.)+.|      Fax : +49-231-755 2450        |
 * \------------------  \|    J   |/  -----------------------------------/
 *                       \   ---  /
 *                        \      /
 *                         "####"
 */

/*
 *	getparam.c
 *	----------
 *	
 *	Read parameters from a given input file, and from the command line.
 *	If the same parameter is defined twice, the one from the command
 *	line is taken.
 */

#include <stdio.h>
#include <ctype.h>
#include <string.h>

#include "getparam.h"

#if !defined(EXIT_FAILURE)
#define EXIT_FAILURE 1
#endif

extern param_t *param;

static char stypes[][MAXKEY] = {
	"string",
	"real",
	"integer",
	"boolean",
	"enum"
};

#define GETINT(p,v,r) (sscanf((v),"%d",(int *)((char *)(r)+(p)->offset)))
#define GETSTR(p,v,r) (sscanf((v),"%s",(char *)((char *)(r)+(p)->offset)))
#define GETDBL(p,v,r) (sscanf((v),"%lg",(double *)((char *)(r)+(p)->offset)))
#define GETBLN(p,v,r) (GETENM((p),(v),(r)))

int
GETENM(param_t *p, char *v, void *r)
{
    char *s, *s1;
    int pos;

    pos = 0;
	
    if ((s1 = (char *) malloc(strlen(p->val)+1)) == NULL) {
	fprintf(stderr,"memory allocation failed in %s\n", __FILE__);
	exit(EXIT_FAILURE);
    }
    strcpy(s1, p->val);
    s = strtok(s1, "|");
    while ( strncmp(s, v, strlen(s)) &&  (s = strtok(NULL,"|")))
	pos++;

    free(s1);

    if (s == NULL) {
	return 0;
    } else {
	*((int *)((char *)(r)+(p)->offset)) = pos;
	return 1;
    }
}

void
Usage(param_t *p, char *s)
{
    param_t *tmp;
    int h, key_width=0, dsc_width=0;

    for (tmp = p+2; tmp->key[0] != '\0'; tmp++) {
	if ((h = strlen(tmp->key)) > key_width) key_width = h;
	if ((h = strlen(tmp->dsc)) > dsc_width) dsc_width = h;
    }
    fprintf(stderr, "# usage: %s param-file {key=value}\n\n", s);
    fprintf(stderr, "# valid keys are (key, description, type, values):\n");
    fprintf(stderr, "#\n");
    for (p += 2; p->key[0] != '\0'; p++)
	fprintf(stderr, "%-*s %c \"%*s\" %-10s %-s\n", 
		key_width, p->key, p->set?'*':'!',
		dsc_width, p->dsc, stypes[p->type], p->val);
    exit(EXIT_FAILURE);
}

void
ParamError(param_t * p, char *s)
{
    fprintf(stderr, "%s: format error\n", s);
    fprintf(stderr, "	Parameter   : %s\n", p->key);
    fprintf(stderr, "	Type        : %s\n", stypes[p->type]);
    fprintf(stderr, "	Description : %s\n", p->dsc);
    fprintf(stderr, "	Values      : %s\n", p->val);
    fprintf(stderr, "	Mandatory   : %s\n", p->set?"no":"yes");

    exit(EXIT_FAILURE);
}

void
getparam(void *result, param_t * param, char **argv)
{
    FILE *f;
    param_t *p;
    char *name, config[256];
    char key[MAXKEY], value[MAXKEY];
    char line[2 * MAXKEY];
    char *ptr;
    int ok;


    sscanf(*argv, "%s", (char *) ((char *) result + param[0].offset));
    name = *argv++;
    if (*argv == NULL)
	Usage(param, name);

    sscanf(*argv, "%s", (char *) ((char *) result + param[1].offset));

    sprintf(config, "%s.in", *argv);
    if ((f = fopen(config, "r")) == NULL) {
	if ((f = fopen(*argv, "r")) == NULL) {

	    fprintf(stderr, "%s: cannot open configuration "
		    "file %s[.in]\n", name, *argv);
	    exit(EXIT_FAILURE);
	}
    }
    argv++;

    for (;;) {
	value[0] = '\0';
	if (f && fgets(line, 2 * MAXKEY + 1, f) != NULL) {
	    for (ptr = line; *ptr && isspace(*ptr); ptr++);
	    if (!*ptr || *ptr == '#')
		continue;

	    if (sscanf(line, "%[^=\t ] = %[^=\t# ]", key, value) == 0) {
		fprintf(stderr, "%s: format error in %s:\n"
			">>> %s\n", name, config, line);
	    }
	} else {
	    if (*argv == NULL)
		break;
	    if (sscanf(*argv++, "%[^=\t ] = %[^=\t# ]", key, value) == 0)
		Usage(param, name);
	}
	ok = 0;
	for (p = param + 2; p->key[0] != '\0' && !ok; p++) {
	    if (strncmp(p->key, key, MAXKEY) == 0) {
		switch (p->type) {
		case INT:
		    if (!GETINT(p, value, result))
			ParamError(p, name);
		    break;

		case STRING:
		    if (!GETSTR(p, value, result))
			ParamError(p, name);
		    break;

		case ENUM:
		    if (!GETENM(p, value, result))
			ParamError(p, name);
		    break;

		case DOUBLE:

		    if (!GETDBL(p, value, result))
			ParamError(p, name);
		    break;

		case BOOLEAN:
		    if (!GETBLN(p, value, result))
			ParamError(p, name);
		    break;

		default:
		    fprintf(stderr,
			    "%s: Undefined parameter "
			    "type for %s\n", name, p->key);
		    break;
		}
		p->set = ok = 1;
	    }
	}
	if (!ok)
	    Usage(param, name);
    }
    ok = 1;
    for (p = param + 2; p->key[0] != '\0'; p++) {
	if (!p->set) {
	    fprintf(stderr, "parameter '%s' not set\n", p->key);
	    ok = 0;
	}
    }
    if (!ok)
	exit(EXIT_FAILURE);
    if (f)
	fclose(f);
}
