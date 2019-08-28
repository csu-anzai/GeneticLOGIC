/*
 * Not def'd anywhere useful
 */
#define CPUSTATES 4

#ifdef sun
#define DK_NDRIVE 4
#endif

#include <pwd.h>
#ifdef hp
#include <time.h>
#endif
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <rpcsvc/rstat.h>
#include <utmp.h>
#include <rpcsvc/rusers.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>
#include <sgtty.h>
#include <signal.h>
#include <errno.h>


/*
 * Also not cleanly available without includeing almost all of sys
 */
#define FSCALE (1<<8)

/*
 * A reource is loik dis...
 */
typedef struct resource {
	char *hostname;
	int	nusers;
	struct statsswtch statp;
} ResourceRecord;

#define SR sizeof(ResourceRecord)

/*
 * A user is something simpler
 */
typedef struct user {
	struct passwd *pwd;
	char pass[120];
} User;

#ifdef pyr
extern int errno;
#endif

