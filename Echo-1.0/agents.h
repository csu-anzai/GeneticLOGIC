/* $Header: /tmp_mnt/vida/disks/disk5/Users/terry/r/echo/RCS/agents.h,v 1.6 1992/11/16 07:05:45 terry Exp terry $ */
extern AGENT *create_agent();
extern AGENT_EDIT *new_agent_edit();
extern BOOLEAN can_self_replicate();
extern void calculate_makeup();
extern BOOLEAN can_pay_maintenance();
extern COUNT mutate_agent();
extern void print_agent();
extern AGENT *agent_self_replicate();
extern void write_agent_to_file();
extern void write_agent_edit_to_file();
extern AGENT *read_agent_from_file();
extern AGENT_EDIT *read_agent_edit_from_file();
extern void migrate_agent();
extern void make_agent_genome();
extern AGENT_POPULATION_SIZE agent_instance();
extern int agent_distance();
extern void agent_crossover();

/* I leave these as lower case since it's safe to think of them as functions. */
#define copy_agent(agent)                     create_agent((agent))
#define new_agent()                           create_agent((AGENT *)0)
