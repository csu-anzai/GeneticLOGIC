{
    "name",
	"Program Name",
	STRING,
	".+",
	MANDATORY,
	offsetof(Field, name)
},
{
    "run",
	"Experiment Name",
	STRING,
	".+",
	MANDATORY,
	offsetof(Field, run)
},
{
    "run",
	"Experiment Name",
	STRING,
	".+",
	OPTIONAL,
	offsetof(Field, run)
},
{
    "mu",
	"No Of Parents (mu)",
	INT, 
	">0",
	MANDATORY,
	offsetof(Field, mu)
},
{
    "lambda",
	"No Of Offspring (lambda)",
	INT, 
	">0",
	MANDATORY,
	offsetof(Field, lambda)
},
{
    "vars", 
	"No Of Variables", 
	INT, 
	">0", 
	MANDATORY,
	offsetof(Field, vars)
},
{
    "sigmas",
	"No Of Sigmas", 
	ENUM, 
	"1|n", 
	MANDATORY,
	offsetof(Field, sigmas)
},
{
    "select", 
	"Selection Scheme",
	ENUM,
	"plus|comma",
	MANDATORY,
	offsetof(Field, select)
},
{
    "reco_x",
	"Recombination x[i]",
	ENUM,
	"none|discrete|arithmetic",
	MANDATORY,
	offsetof(Field, reco_x)
},
{
    "reco_s",
	"Recombination sigma[i]",
	ENUM, 
	"none|discrete|geometric|arithmetic",
	MANDATORY,
	offsetof(Field, reco_s)
},

{
    "tau0",
	"Global Sigma Variance",
	DOUBLE,
	">0.0",
	MANDATORY,
	offsetof(Field, tau0)
},
{
    "taui",
	"Local Sigma Variance",
	DOUBLE,
	">0.0",
	MANDATORY,
	offsetof(Field, taui)
},
{
    "min_init_x",
	"Lower Boundary For Initial x[i]",
	DOUBLE,
	">0.0",
	MANDATORY,
	offsetof(Field, min_init_x)
},
{
    "max_init_x",
	"Upper Boundary For Initial x[i]",
	DOUBLE,
	">0.0",
	MANDATORY,
	offsetof(Field, max_init_x)
},
{
    "min_init_s",
	"Lower Boundary For Initial sigma[i]",
	DOUBLE,
	">0.0",
	MANDATORY,
	offsetof(Field, min_init_s)
},
{
    "max_init_s",
	"Upper Boundary For Initial sigma[i]",
	DOUBLE,
	">0.0",
	MANDATORY,
	offsetof(Field, max_init_s)
},
{
    "smin",
	"Minimum sigma[i]",
	DOUBLE,
	">0.0",
	MANDATORY,
	offsetof(Field, smin)
},
{
    "smax",
	"Maximum sigma[i]",
	DOUBLE, ">0.0",
	MANDATORY,
	offsetof(Field, smax)
},
{
    "maxgen",
	"Maximum No Of Generations",
	INT,
	">=0",
	OPTIONAL,
	offsetof(Field, maxgen)
},
{
    "maxcalls",
	"Maximum No Of Function Calls",
	INT,
	">=0",
	OPTIONAL,
	offsetof(Field, maxcalls)
},
{
    "dmpint",
	"Dump Interval",
	INT,
	">=0",
	OPTIONAL,
	offsetof(Field, dmpint)
},
{
    "seed",
	"Random Seed",
	INT,
	"0..32768",
	OPTIONAL,
	offsetof(Field, seed)
},
{
    "logfile",
	"Logfile",
	STRING,
	"^[a-zA-Z0-9_\\-\\.]+$",
	OPTIONAL,
	offsetof(Field, logfile)
},
{
    "flushlog",
	"Flush Interval",
	INT,
	">=0",
	OPTIONAL,
	offsetof(Field, flushlog)
},
{
    "pixmon",
	"Use PixMon",
	BOOLEAN,
	"no|yes",
	OPTIONAL,
	offsetof(Field, pixmon)
},
{
    "showxs",
	"Show best's x and sigma",
	BOOLEAN,
	"no|yes",
	OPTIONAL,
	offsetof(Field, showxs)
},
{
    "showy",
	"Show best's f(x)",
	BOOLEAN,
	"no|yes",
	OPTIONAL,
	offsetof(Field, showy)
},
{
    "", "", INT, "", 0, 0
}

