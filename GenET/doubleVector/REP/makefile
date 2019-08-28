SRCS = representation.c operators.c
OBJS = representation.o operators.o

CFLAGS = -O

repAndOpers.o : $(OBJS) 
	$(LD) -r -o repAndOpers.o $(OBJS) 

$(OBJS) : ../../ALG/main.h
	$(CC) $(CFLAGS) -I../../ALG -c $(SRCS) 
