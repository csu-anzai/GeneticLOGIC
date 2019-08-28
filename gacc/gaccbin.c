/*  Genetic Aided Cascade Correlation  */
/*  written by Erik Mayer 3/15/93      */
/*  EMAYER@uoft02.utoledo.edu          */

/*  For binary outputs */
	
#include <stdio.h>
#include <math.h>
#include <signal.h>
#include <string.h>

#undef    DBCROSS	/* crossover debug */

typedef int BOOLEAN;    /* define type BOOLEAN */

/* NNet Parameters */

int iterations = 1;     /* display iteration interval */
int numinput  =	4;	/* number of input nodes      */
int nummiddle =	2;	/* number of middle nodes     */
int numhidden = 0;	/* numbe of hidden nodes      */		
int numoutput =	1;	/* number of output nodes     */

int net_size;           /* number of neurons in nnet  */
int num_weights;        /* number of weights in nnet  */
int chromo_length;      /* length of chromosome in bytes */
int bits_per_weight;	/* number of bits per weight  */
float min_weight;	/* minimum weight value	      */	
float max_weight;	/* maximum weight value       */
float learning_rate;    /* learning rate for backprop */
float momentum;		/* momentum term for backprop */	
float ga_tol=1000.0;

float* input;  /* input array */

float tol;				/* error tolerance */
int numtrain  =	10;			/* number of training vectors */
float **train_input, **train_output;    /* training arrays 	      */

/* GA Parameters  */

int   pop_size = 200;   	/*  population size       */
int   niche_factor = 0;		/*  niche factor	  */
float pcrossover  = 0.05;  	/*  crossover probability */
float pmutate   = 0.005; 	/*  mutation probability  */

int   patience = 100;	/* number of iterations til new neuron added */

FILE* f;    /*  training file  */
FILE* g;    /*  record file    */
FILE* h;    /*  testing file   */

/* Neural Network structures */

typedef struct synapse{
		float* connect;     	 	/* input neuron connection  */ 
		float weight;	   	 	/* weight of connection	    */
		float deltas;
		float slopes;
                float prevslopes;
		float prevdelta;                /* previous weight change   */
		struct synapse *nextsyn; 	/* pointer to next synapse  */ 
	      } SYNAPSE;

typedef struct neuron{
		float out;	    		/* ouput of neuron          */
		float delta;                    /* delta for backprop       */
		SYNAPSE* synlist;   		/* pointer to first synapse */
		struct neuron *nextneur;   	/* pointer to next neuron   */
 	      } NEURON;

typedef char CHROMO;

typedef struct{
		NEURON* hiddenlist;  		/* pointer to hidden neurons */
		NEURON* outputlist;  		/* pointer to output neurons */
		CHROMO* chromosome;		/* pointer to chromosome     */
		NEURON** output;	    	/* pointer to output neurons */
		NEURON** middle;	    	/* pointer to middle neurons */
	        float fitness;	    		/* fitness of nnet           */	
		float sse;			/* sum squares of error      */
	       } NNET;

typedef NNET* POP;	/* pointer to population of nnets */

/* networks holding max and min wts for each synapse */
NNET minwt;
NNET maxwt;

/* pseudo-random number generators */

int rndseed=0;
int pseudo()	/* returns random number between 0 and Modulus-1 */
{
    rndseed = ( 40*rndseed + 3641 ) % 729;
    return(rndseed);
}	

float random()  /* returns random number from 0 to less than 1 */
{
    return( (float)pseudo() / 729.0 );
}

char randomchar()  /* returns random character */
{
    char randchar=0x00,k = 0x01;   
    int i;

    for(i=0;i<8;i++){
	if(pseudo() > 364) randchar |= k;
	k <<= 1;
    }	
    
    return(randchar);	    
}	

SYNAPSE* syn_alloc(int i)	/* allocates memory for a synapse */
{
    return( (SYNAPSE *)calloc(i,sizeof(SYNAPSE)) );
}    

NEURON* neuron_alloc(int i)      /* allocates memory for a neuron */
{
    return( (NEURON *)calloc(i,sizeof(NEURON)) );
}  

CHROMO* chromo_alloc(int i)      /* allocates memory for chromosome */
{
    return( (CHROMO *)calloc(i,sizeof(CHROMO)) );
}	

NNET* net_alloc(int i)           /* allocates memory for a nnet   */
{
    return( (NNET *)calloc(i,sizeof(NNET)) );
}

float* input_alloc() /* allocates input array */
{
    float *input;
    int i;

    input = (float *)calloc(numinput+1,sizeof(float)); 

    input[0] = 1.00; /* bias connection*/
    for(i=1;i<numinput+1;i++) input[i]=0.0;

    return(input);
}

void quit(int code)
{
    printf("Quitting already?");
    exit(-1);
}

float **create_float_array(int rows, int columns)
{
    float **x;
    int i;

    x = (float **)calloc(rows,sizeof(float *));
    for(i=0;i<rows;i++)
	x[i] = (float *)calloc(columns,sizeof(float));
    
    return(x);
}

begin_stats()
{

 	printf("Neural Net Parameters:\n");
 	printf("  #input nodes: %d\n",numinput);
 	printf("  maximum #hidden nodes: %d\n",nummiddle);
	printf("  #output nodes: %d\n",numoutput);
	printf("  minimum weight: %f\n",min_weight);
	printf("  maximum weight: %f\n",max_weight);
	printf("  learning rate: %f\n",learning_rate);
	printf("  momentum: %f\n",momentum);
	printf("  tolerance: %f\n",tol);
	printf("\n");
 	printf("GA Parameters:\n");
	printf("  population size: %d\n",pop_size);
	printf("  number of bits per weight: %d\n",bits_per_weight);
        printf("  crossover probability: %f\n",pcrossover);
       	printf("  mutation probability: %f\n",pmutate); 
	printf("  niche factor: %d\n",niche_factor);
	printf("  patience: %d\n",patience);
	printf("\n");	

 	fprintf(g,"Neural Net Parameters:\n");
 	fprintf(g,"  #input nodes: %d\n",numinput);
 	fprintf(g,"  maximum #hidden nodes: %d\n",nummiddle);
	fprintf(g,"  #output nodes: %d\n",numoutput);
	fprintf(g,"  minimum weight: %f\n",min_weight);
	fprintf(g,"  maximum weight: %f\n",max_weight);
	fprintf(g,"  learning rate: %f\n",learning_rate);
	fprintf(g,"  momentum: %f\n",momentum);
	fprintf(g,"  tolerance: %f\n",tol);
	fprintf(g,"\n");
 	fprintf(g,"GA Parameters:\n");
	fprintf(g,"  population size: %d\n",pop_size);
	fprintf(g,"  number of bits per weight: %d\n",bits_per_weight);
        fprintf(g,"  crossover probability: %f\n",pcrossover);
       	fprintf(g,"  mutation probability: %f\n",pmutate); 
	fprintf(g,"  niche factor: %d\n",niche_factor);
	fprintf(g,"  patience: %d\n",patience);
	fprintf(g,"\n");	

	fclose(g);

}

load_training()  /* loads parameters, training vectors, and allocates inputs */
{

    int i,j,k;
    char train_file[30];	
    char test_file[30];	
    char out_file[30];	

    printf("\n");	
    printf("\n");	
    printf("Enter training file: ");
    scanf("%s%*c",train_file);

    if((f = fopen(train_file,"r")) == NULL) {
	printf("\007Error opening training file!\n");
    }


    if((g = fopen("gacc.out","w")) == NULL) 
	printf("\007Error opening output file!\n");

    printf("Enter test file: ");
    scanf("%s%*c",test_file);

    if((h = fopen(test_file,"r")) == NULL) {
	printf("\007Error opening testing file!\n");
    }

    fprintf(g,"Train file: %s\n",train_file);	
    fprintf(g,"Test file: %s\n",test_file);

    fscanf(f,"%*s%d\n",&numinput);
    fscanf(f,"%*s%d\n",&nummiddle);
    fscanf(f,"%*s%d\n",&numoutput);	
    fscanf(f,"%*s%f\n",&min_weight);	
    fscanf(f,"%*s%f\n",&max_weight);	
    fscanf(f,"%*s%f\n",&learning_rate);
    fscanf(f,"%*s%f\n",&momentum);
    fscanf(f,"%*s%f\n",&tol);	
    fscanf(f,"%*s%d\n",&pop_size);
    fscanf(f,"%*s%d\n",&bits_per_weight);	
    fscanf(f,"%*s%f\n",&pcrossover);
    fscanf(f,"%*s%f\n",&pmutate);
    fscanf(f,"%*s%d\n",&niche_factor);	
    fscanf(f,"%*s%d\n",&patience);
    fscanf(f,"%*s%d\n",&numtrain);
    
    input = input_alloc();
    train_input = create_float_array(numinput,numtrain);
    train_output = create_float_array(numoutput,numtrain);

    for(i=0;i<numtrain;++i){
	for(j=0;j<numinput;++j) fscanf(f,"%f",&train_input[j][i]);
	for(j=0;j<numoutput;++j) fscanf(f,"%f",&train_output[j][i]);    
    }	    

    printf("\n");	
/*
    printf("Training data:\n");
    printf("\n");    

    printf("Input:");	
    for(j=1;j<numinput;++j) printf("      ",train_input[j][i]);
    printf("| Output:\n");

    for(i=0;i<numtrain;++i){
	for(j=0;j<numinput;++j) printf("%5.3f ",train_input[j][i]);
	printf("| ");
	for(j=0;j<numoutput;++j) printf("%5.3f ",train_output[j][i]);
        printf("\n");    
    }	    
*/
    printf("\n");    
}

byte_disp(unsigned int j) 
{
    int i;
    unsigned int x=0x80;
    for(i=0;i<8;i++){
	(x&j) ? printf("1") : printf("0");
	x >>= 1;
    }
}

fbyte_disp(unsigned int j) 
{
    int i;
    unsigned int x=0x80;
    for(i=0;i<8;i++){
	(x&j) ? fprintf(g,"1") : fprintf(g,"0");
	x >>= 1;
    }
}

byte_new_disp(unsigned int j)
{

    int i;

    for(i=0;i<8;i++) printf("%d",bit_decode(j,i));		

}

BOOLEAN bit_decode(unsigned int j,int pos) 
{

    int i;
    unsigned int x=0x80;

    for(i=0;i<pos;i++) x >>= 1;
    return( (x&j)?1:0 );		

}

bit_encode(char *j,int pos,BOOLEAN bit) 
{
	
    int i;
    unsigned int x=0x80,*y;

    y = (unsigned int *)j;	

    for(i=0;i<pos;i++) x >>= 1;

    if(bit) *y |= x;
    else    *y &= ~x; 		

}

input_disp()  /* displays input array */
{

    int i;

    printf("Input Array:\n");
    for(i=0;i<numinput+1;i++) 
	printf(" %d %d %5.2f\n",i,&input[i],input[i]);

}

neuron_disp(NEURON *neur) /* shows weights and bias */
{

    SYNAPSE *syn;

    printf("  neuron: %d\n",neur);
    for(syn=neur->synlist;syn!=NULL;syn=syn->nextsyn){ 
    	printf("    %-10.5f   %6d\n",syn->weight,syn->connect);
    }	    

}

fneuron_disp(NEURON *neur) /* shows weights and bias */
{

    SYNAPSE *syn;

    fprintf(g,"  neuron: %d\n",neur);
    for(syn=neur->synlist;syn!=NULL;syn=syn->nextsyn){ 
    	fprintf(g,"    %-10.5f   %6d\n",syn->weight,syn->connect);
    }	    

}

neuron_vt100_disp(NEURON *neur) /* shows weights and bias */
{

    SYNAPSE *syn;

    printf("  neuron: %d\n",neur);
    printf("    synlist:\n");
    for(syn=neur->synlist;syn!=NULL;syn=syn->nextsyn){
    	printf("    %-10.5f   %6d\n",syn->weight,syn->connect);
    }	    

}

nnet_disp(NNET *net0) /* display weights, rms error of a network */
{

    int i;
    NEURON *neur;

    printf("Hidden units:\n");
    for(neur=net0->hiddenlist;neur!=NULL;neur=neur->nextneur) 
	neuron_disp(neur);
    printf("Output units:\n");	
    for(neur=net0->outputlist;neur!=NULL;neur=neur->nextneur) 
	neuron_disp(neur);
    printf("\n");	
/*    for(i=0;i<numoutput;i++)
        printf("  outputneuron#: %d\n",net0->output[i]);*/
    printf("  fitness: %f sse: %f\n",net0->fitness,net0->sse);
    if(net0->chromosome!=NULL) chromo_disp(net0->chromosome);
    printf("\n");   	

}

fnnet_disp(NNET *net0) /* display weights, rms error of a network */
{                      /* display output wts and last hidden neuron */

    int i;
    NEURON *neur;

    neur=net0->hiddenlist;

    if(neur!= NULL){ 
        fprintf(g,"Hidden units:\n");
	for(;neur->nextneur!=NULL;neur=neur->nextneur); 
    	fneuron_disp(neur);
	fprintf(g,"\n");
    }
    fprintf(g,"Output units:\n");	
    for(neur=net0->outputlist;neur!=NULL;neur=neur->nextneur) 
	fneuron_disp(neur);
   fprintf(g,"\n");	
    for(i=0;i<numoutput;i++)
        fprintf(g,"  outputneuron#: %d\n",net0->output[i]);
    fprintf(g,"  fitness: %f sse: %f\n",net0->fitness,net0->sse);
    fchromo_disp(net0->chromosome);
    fprintf(g,"\n");	

}


nnet_bin_disp(NNET *net0) /* display chromosome, rms error of a network */
{
    int i;
    
    chromo_disp(net0->chromosome);
    printf("  fitness: %f\n",net0->fitness);
}

pop_rank(POP pop1)  /* assign fitness according to rank */
{

    NNET temp;
    int i,j;

    for(i=0;i<pop_size-1;i++){
        for(j=i+1;j<pop_size;j++){
            if(pop1[j].sse > pop1[i].sse){
		temp = pop1[i];
		pop1[i] = pop1[j];
 		pop1[j] = temp;			
	    }
        }
    }

/*    for(i=0;i<pop_size;i++)
	pop1[i].fitness = i+1;	
*/

}

pop_disp2(POP pop1) /* display population sse and chromosomes */
{

    int i;

    for(i=0;i<pop_size;i++){
	nnet_disp(&pop1[i]);
	printf("%2d %8.2f %8.2f ",i,pop1[i].sse,pop1[i].fitness);
	printf("\n");
    }

}

pop_disp(POP pop1) /* display population sse and chromosomes */
{

    int i;

    for(i=0;i<pop_size;i++){
	printf("%3d",i);
	nnet_dispx(&pop1[i]);
	printf(" %8.5f %8.5f",pop1[i].sse,pop1[i].fitness);
	printf("\n");
    }

}

nnet_dispx(NNET *net0) /* display weights, rms error of a network */
{

    int i;
    NEURON *neur;

    for(neur=net0->hiddenlist;neur!=NULL;neur=neur->nextneur) 
	neuron_dispx(neur);
    for(neur=net0->outputlist;neur!=NULL;neur=neur->nextneur) 
	neuron_dispx(neur);
    printf(" ");	
    chromo_disp(net0->chromosome); 

}

neuron_dispx(NEURON *neur) /* shows weights and bias */
{

    SYNAPSE *syn;
    printf("%d",neur);
    for(syn=neur->synlist;syn!=NULL;syn=syn->nextsyn){ 
    	printf(" %-6.2f",syn->weight);
    }	    
    printf(" ");
}

chromo_random(CHROMO *chromosome) /* randomize weights and bias */
{

    int i;

    for(i=0;i<chromo_length;i++) chromosome[i]=randomchar();	    

}

chromo_disp(CHROMO *chromosome) /* display weights, rms error of a network */
{
    int i;
    
    printf("[");
    for(i=0;i<chromo_length;i++) byte_disp(chromosome[i]);	
    printf("]");

}

fchromo_disp(CHROMO *chromosome) /* display weights, rms error of a network */
{
    int i;
    
    fprintf(g,"[");
    for(i=0;i<chromo_length;i++) fbyte_disp(chromosome[i]);	
    fprintf(g,"]");

}

int chromo_compare(CHROMO *chrom1, CHROMO *chrom2) /* compare chromosomes */
{

    int i,j,k,l,match;
    unsigned int x;

    for(i=0;i<chromo_length;i++){
    	x=0x80;
    	for(l=0;l<8;l++){
    		j = (x&chrom1[i])?1:0;
    		k = (x&chrom2[i])?1:0;		
		if(j==k) ++match;
                x >>= 1;
	}
    }	

    return(match);	

}

float chromo_decode(CHROMO *chromosome,int i) /* decode chromosome */
{         /* i = 0 to num_weights-1 */

    float weight=0.0,scale=1.0;
    int byte_number,bit_position,k;
    BOOLEAN j;	

    byte_number = i * bits_per_weight / 8;	
    bit_position = i * bits_per_weight % 8;		

    for(j=0;j<chromo_length;j++){
/*	byte_disp(chromosome[j]);
	printf(":");  */
    }  	
/*    printf("\nbyte: %d  bit: %d\n",byte_number,bit_position); */	


    for(k=0;k<bits_per_weight;bit_position++,k++){
	if(bit_position==8){
		bit_position=0;
		++byte_number;
	}		
	j = bit_decode(chromosome[byte_number],bit_position);
	scale *= 2.0;
/*	printf("%d %d : ",byte_number,bit_position);
    	printf("%d\n",j);    */
    
	weight = 2.0 * weight + j;
    }			

/*    printf("raw weight = %f\n",weight);  */	

    scale -= 1;

/*    printf("scale: %f\n",scale);  */	

    /* scale weight */	

    weight = min_weight + weight*(max_weight-min_weight)/scale;		

/*    printf("unscaled weight = %f\n",weight);  */	

    return(weight);	

}

chromo_encode(CHROMO *chromosome,int i,float weight) /* encode chromosome */
{         /* i = 0 to num_weights-1   8/20/92  */

    float scale=1.0;
    int byte_number,bit_position,k,wt;
    BOOLEAN j;	

    for(k=0;k<bits_per_weight;k++) scale *= 2.0;
    scale -= 1;

    /* scale weight */	

    wt = (weight - min_weight)/(max_weight-min_weight)*scale+0.5;		

    byte_number =  (i * bits_per_weight + bits_per_weight-1) / 8;	
    bit_position = (i * bits_per_weight + bits_per_weight-1) % 8;		

/*    printf("chromo_encode\n"); */

/*    for(j=0;j<chromo_length;j++){
	byte_disp(chromosome[j]);
	printf(":");
    }  	
    printf("\nbyte: %d  bit: %d\n",byte_number,bit_position);	
    printf("weight: %f  ",weight);	
    printf("scaled wt: %d\n",wt);   */	

    for(k=bits_per_weight;k>0;bit_position--,k--){
	if(bit_position==-1){
		bit_position=7;
		--byte_number;
	}		

	bit_encode(&chromosome[byte_number],bit_position,wt%2);
	
/*	printf("%d %d : ",byte_number,bit_position);
    	printf("%d\n",wt%2);  */ 
 	wt /= 2;
    
    }			

}

nnet_wt_from_chromo(NNET *net0) /* map weights from chromosome */
{

    int i=0;

    NEURON *neur,*minneur,*maxneur;
    SYNAPSE *syn,*minsyn,*maxsyn;

    minneur=minwt.outputlist;
    maxneur=maxwt.outputlist;

/*	printf("check 1\n");  */
    
    for(neur=net0->outputlist;neur!=NULL;neur=neur->nextneur){
	minsyn=minneur->synlist;
	maxsyn=maxneur->synlist; 
    	for(syn=neur->synlist;syn!=NULL;syn=syn->nextsyn,i++){
		min_weight = minsyn->weight;
		max_weight = maxsyn->weight;
		syn->weight = chromo_decode(net0->chromosome,i);

/*                printf("syn->connect: %d\n",syn->connect); */

/*		printf("map_weights\n");
		printf("%d weight = %f \n",i,syn->weight); */ 

                minsyn = minsyn->nextsyn;
                maxsyn = maxsyn->nextsyn;
	}
	minneur=minneur->nextneur;
	maxneur=maxneur->nextneur;
    }    
}

nnet_chromo_from_wt(NNET *net0) /* map chromosome from weights */
{

    int i=0;
    NEURON *neur;
    SYNAPSE *syn;
    
    for(neur=net0->outputlist;neur!=NULL;neur=neur->nextneur) 
    	for(syn=neur->synlist;syn!=NULL;syn=syn->nextsyn,i++){
		chromo_encode(net0->chromosome,i,syn->weight);
      /*    	printf("%d weight = %f \n",i,syn->weight);  */
	}    
}

nnet_cc_create(NNET *net0)  /* creates a cascade correlated network   */
{                           /* allocates chromosome, randomizes it,   */

    int i,j;                /* allocates output neurons, link  to     */
    NEURON *neur;
                            /* inputs                                 */ 
                            /* maps weights from chromosome.          */

    num_weights = numoutput*(numinput+1);
    net_size = numoutput;

    /* find number of bytes needed for chromosome */

    chromo_length = ( num_weights * bits_per_weight / 8.0 + .9 );		

    /* allocate chromosome */

    net0->chromosome = chromo_alloc(chromo_length);		

    /* randomize chromosome */

    chromo_random(net0->chromosome);			

    /* allocate output neurons for network and form neuron link list */
    /* allocate synapses, form synapse link list, */
    /* and connect synapse to bias */

    nnet_create_noc(net0);	
	
    /* allocate output pointer array:  net0->output[i] */
    net0->output = (NEURON**)calloc(numoutput,sizeof(NEURON*));

    for(neur=net0->outputlist,i=0;neur!=NULL;neur=neur->nextneur,i++)
        net0->output[i] = neur;

    /* map chromosome to weights  why? */

    nnet_wt_from_chromo(net0);
	
}

nnet_create_noc(NNET *net0)  
{

/************************************** 
      creates a 1 layer network:    
  	inputs connected to output
        weights = 0.0
	no chromosome
****************************************/

    int i,j;

    NEURON *neur;
    SYNAPSE *syn;
	
    net0->hiddenlist = NULL;	
 
    neur = net0->outputlist = neuron_alloc(1);	

    /* connect to inputs */

    for(j=0;j<numinput+1;j++){
        synapse_connect(&input[j],neur);
    }

    for(i=1;i<numoutput;i++){

	neur = neur->nextneur = neuron_alloc(1);

	/* connect to inputs */

	for(j=0;j<numinput+1;j++){
	    synapse_connect(&input[j],neur);
	}

    }

    neur->nextneur = NULL;	

    /* allocate output pointer array:  net0->output[i] */
    net0->output = (NEURON**)calloc(numoutput,sizeof(NEURON*));

    for(neur=net0->outputlist,i=0;neur!=NULL;neur=neur->nextneur,i++)
        net0->output[i] = neur;


}

nnet_cc_candidate(NNET *net0,NNET *best)  /* creates cc candidates */
{

    int i,j;
    NEURON *neur;
    SYNAPSE *syn;

    num_weights = (numinput+1+numhidden);
    net_size = 1;

    /* find number of bytes needed for chromosome */

    chromo_length = ( num_weights * bits_per_weight / 8.0 + .9 );		

    /* allocate chromosome */

    net0->chromosome = chromo_alloc(chromo_length);		

    /* randomize chromosome */

    chromo_random(net0->chromosome);			

    /* allocate output neurons for network and form neuron link list */
    /* allocate synapses, form synapse link list, */
    /* and connect synapse to bias */
 
    neur = net0->outputlist = neuron_alloc(1);	
    neur->nextneur = NULL;

	/* connect to inputs */

	syn = neur->synlist = syn_alloc(1);
	syn->connect = &input[0];

	for(j=1;j<numinput+1;j++){
	    syn = syn->nextsyn = syn_alloc(1);
	    syn->connect = &input[j];
	}

	syn->nextsyn = NULL;	
	
	/* connect to hidden nodes */

	for(neur=best->hiddenlist;neur!=NULL;neur=neur->nextneur){
	    syn = syn->nextsyn = syn_alloc(1);
/*	    printf("hidden %d\n",(float *)neur);   */	
	    syn->connect = (float *)neur;
	}			

	syn->nextsyn = NULL;	

    /* allocate output pointer array:  net0->output[i] */
    net0->output = (NEURON**)calloc(numoutput,sizeof(NEURON*));

    for(neur=net0->outputlist,i=0;neur!=NULL;neur=neur->nextneur,i++)
        net0->output[i] = neur;
	
    nnet_wt_from_chromo(net0);
	
}


nnet_add_cc_neuron2(NNET *net0,NNET *net1)
{

    /* add neuron net1 to network net0 */	

    int i,j;
    NEURON *neur0,*neur1;
    SYNAPSE *syn;
	
    num_weights = numoutput*(numhidden + numinput + 1);

    /* find number of bytes needed for chromosome */

    chromo_length = ( num_weights * bits_per_weight / 8.0 + .9 );		

    /* reallocate chromosome */
	
    cfree(net0->chromosome);

    net0->chromosome = (CHROMO *)calloc(chromo_length,sizeof(CHROMO));

    /* randomize chromosome */

    chromo_random(net0->chromosome);			

    /* link neuron to neurons in best's hiddenlist */

/*    neur0=net0->hiddenlist;
    neur1=net1->outputlist;	

    for(i=0,syn=neur1->synlist;syn->nextsyn!=NULL;syn=syn->nextsyn,i++){
	printf("%d %d\n",i,syn->connect);	{    
	if(i>numinput+1){
		syn->connect = neur0;
		neur0=neur0->nextneur;
	}
    }	
*/

    /* add neuron to best's hiddenlist */
	
    if(net0->hiddenlist == NULL) net0->hiddenlist = net1->outputlist;	

    else {
	for(neur0=net0->hiddenlist;neur0->nextneur!=NULL;neur0=neur0->nextneur);
	neur0->nextneur = net1->outputlist;
    }

    /* connect to output neurons in outputlist */

    for(neur0=net0->outputlist;neur0!=NULL;neur0=neur0->nextneur){

	for(syn=neur0->synlist;syn->nextsyn!=NULL;syn=syn->nextsyn);	
	syn = syn->nextsyn = syn_alloc(1);
	syn->connect = (float*)net1->outputlist;

    }
	
    	syn->nextsyn = NULL;

    nnet_chromo_from_wt(net0);
	
}

nnet_add_cc_neuron(NNET *net0,NNET *net1)
{
    /* 2/5/93 */
    /* add neuron net1 to network net0 */	

    int i,j;
    NEURON *neur0,*neur1;
    SYNAPSE *syn;
	
    num_weights = numoutput*(numhidden + numinput + 1);

    /* find number of bytes needed for chromosome */

    chromo_length = ( num_weights * bits_per_weight / 8.0 + .9 );		

    /* reallocate chromosome */
	
    cfree(net0->chromosome);

    net0->chromosome = (CHROMO *)calloc(chromo_length,sizeof(CHROMO));

    /* randomize chromosome */

    chromo_random(net0->chromosome);			

    /* link neuron to neurons in best's hiddenlist */

/*    neur0=net0->hiddenlist;
    neur1=net1->outputlist;	

    for(i=0,syn=neur1->synlist;syn->nextsyn!=NULL;syn=syn->nextsyn,i++){
	printf("%d %d\n",i,syn->connect);	{    
	if(i>numinput+1){
		syn->connect = neur0;
		neur0=neur0->nextneur;
	}
    }	
*/

    /* add neuron to best's hiddenlist */
	
    if(net0->hiddenlist == NULL) net0->hiddenlist = net1->outputlist;	

    else {
	for(neur0=net0->hiddenlist;neur0->nextneur!=NULL;neur0=neur0->nextneur);
	neur0->nextneur = net1->outputlist;
    }

    /* connect to output neurons in outputlist */

    for(neur0=net0->outputlist;neur0!=NULL;neur0=neur0->nextneur){

	for(syn=neur0->synlist;syn->nextsyn!=NULL;syn=syn->nextsyn);	
	syn = syn->nextsyn = syn_alloc(1);
	syn->connect = (float *)net1->outputlist;
    	syn->nextsyn = NULL;

    }
	

    nnet_chromo_from_wt(net0);
	
}

synapse_connect(float *in,NEURON *neur1)
/* adds synapse to end of neuron synapse list and */
/* connects synapse from input to NEURON *neur1  */
{
	
	SYNAPSE *syn;	

	syn=neur1->synlist;

	/* if no synapses in list */

	if(syn == NULL){   
                syn = neur1->synlist = syn_alloc(1);
        }

	/* if synapses in list */

	else{	
		for(;syn->nextsyn!=NULL;syn=syn->nextsyn);	
		syn = syn->nextsyn = syn_alloc(1);
        }

	syn->connect = in;
	syn->weight = 0.0;
    	syn->nextsyn = NULL;
  /* printf("synapse %d connect %d weight%f\n",syn,syn->connect,syn->weight);*/

}

nnet_link_to_candidate(NNET *net0,NNET *cand1)
{

    /* link candidate cand1 to network net0 */	

    int i,j;
    NEURON *neur0,*neur1;
    SYNAPSE *syn;
	
    num_weights = numoutput*(numhidden + numinput + 1);

    /* find number of bytes needed for chromosome */

    chromo_length = ( num_weights * bits_per_weight / 8.0 + .9 );		

/*    printf("realloc\n");  */	

    /* reallocate chromosome */

    cfree(net0->chromosome);

    net0->chromosome = (CHROMO *)calloc(chromo_length,sizeof(CHROMO));

/*    printf("randomize\n"); */

    /* randomize chromosome */

    chromo_random(net0->chromosome); 			
	
/*    printf("connect to cand outputlist\n"); */

    /* connect to output neurons in cand1 outputlist */

    for(neur0=net0->outputlist;neur0!=NULL;neur0=neur0->nextneur){
	for(syn=neur0->synlist;syn->nextsyn!=NULL;syn=syn->nextsyn);	
	syn = syn->nextsyn = syn_alloc(1);
	syn->connect = (float *)cand1->outputlist;
    }
    syn->nextsyn = NULL;

/*    printf("nnet_chromo_from_wt\n");	*/
	
    nnet_chromo_from_wt(net0);
	
}

nnet_propagate(NNET *net0) /* propagate network */
{
    int i;
    NEURON *neur;
                  
    for(neur=net0->hiddenlist;neur!=NULL;neur=neur->nextneur){
	activate_neuron(neur);
    }

    for(neur=net0->outputlist;neur!=NULL;neur=neur->nextneur){
	activate_neuron(neur);
    }

}

nnet_propagate_hidden(NNET *net0) 
/* propagate hidden layer of network 1/23/93 */
{
    int i;
    NEURON *neur;
                  
    for(neur=net0->hiddenlist;neur!=NULL;neur=neur->nextneur){
	activate_neuron(neur);
    }

}

activate_neuron(NEURON *neur)
{  /* sigmoidal 0 to 1 */

    int i,j;
    SYNAPSE *syn;
    float net=0.0;

    for(syn=neur->synlist;syn!=NULL;syn=syn->nextsyn){
	net += *syn->connect * syn->weight;
    }

    neur->out = 1/(1+exp(-net));

}

nnet_train(NNET *net0) /* apply training data to net and calculate sse */
{
    int i,j;
    float error,sse=0.0;
        
    /* set up training data */

    for(j=0;j<numtrain;j++){
	for(i=0;i<numinput;i++) input[i+1]=train_input[i][j];
	nnet_propagate(net0);
	for(i=0;i<numoutput;i++){
        	error = train_output[i][j] - net0->output[i]->out;
		sse += error * error; 
/*		sse += (error<0)?-error:error;  */
	}
    }	
    net0->sse = sse;	

    net0->fitness = 4*numtrain*numoutput - sse;	
}

nnet_test(NNET *net0) /* apply test data to net and calculate sse */
{

    int i,j,k;

/*        input = input_alloc();  */

    fscanf(h,"%*s%d\n",&numtrain);

/*    train_input = create_float_array(numinput,numtrain);
    train_output = create_float_array(numoutput,numtrain);
*/

    for(i=0;i<numtrain;++i){
	for(j=0;j<numinput;++j) fscanf(h,"%f",&train_input[j][i]);
	for(j=0;j<numoutput;++j) fscanf(h,"%f",&train_output[j][i]);    
    }	    

    printf("\n");	

    printf("Applying test data...\n");
    printf("\n");    

/*    printf("Input:");	
    for(j=1;j<numinput;++j) printf("      ",train_input[j][i]);
    printf("| Output:\n");

    for(i=0;i<numtrain;++i){
	for(j=0;j<numinput;++j) printf("%5.3f ",train_input[j][i]);
	printf("| ");
	for(j=0;j<numoutput;++j) printf("%5.3f ",train_output[j][i]);
        printf("\n");    
    }	    
*/
    printf("\n");    
	
	nnet_train(net0);
	fnnet_train_disp(net0);

}

nnet_cc_train(NNET *net0,NNET *best) 
/* apply training data to net and calculate fitness */
{
    int i,j,k;
    float sumvp=0.0,sumerror=0.0,summult=0.0,error,fitness=0.0;

    net0->fitness = 0.0;
	        
    for(i=0;i<numoutput;i++){
    	sumvp=0.0;sumerror=0.0;summult=0.0;
    	for(j=0;j<numtrain;j++){
		for(k=0;k<numinput;k++)input[k+1]=train_input[k][j];

        	nnet_propagate(best);
		nnet_propagate(net0);

        	error = train_output[i][j] - best->output[i]->out;

		sumerror += error; 
                sumvp += net0->output[i]->out;
		summult += error*net0->output[i]->out;
	}

    	fitness = summult - sumvp*sumerror/(float)numtrain;	

        net0->fitness += (fitness<0) ? (-fitness) : fitness;

    }	
}

nnet_cc_update(NNET *net0,NNET *best) 
/* apply training data to candidate and modify weights */
{

    int i,j,k;
    float sumvp=0.0,sumerror=0.0,summult=0.0,error,fitness=0.0,sumdeltao=0.0;
    float fprime,corrsign,erroravg,sumfprpipep=0.0,sumfprpip=0.0,sumdelta=0.0;	
    SYNAPSE *syn,*syn2,*syn3;		
	
    net0->fitness = 0.0;
	        
    syn2=minwt.outputlist->synlist;
    syn3=maxwt.outputlist->synlist;

    for(syn=net0->outputlist->synlist;syn!=NULL;syn=syn->nextsyn){	

	syn2->weight = syn->weight - ga_tol*syn->weight;
	syn3->weight = syn->weight + ga_tol*syn->weight;
    }

}

nnet_cc_update2(NNET *net0,NNET *best) 
/* apply training data to candidate and modify weights */
{

    int i,j,k;
    float sumvp=0.0,sumerror=0.0,summult=0.0,error,fitness=0.0,sumdeltao=0.0;
    float fprime,corrsign,erroravg,sumfprpipep=0.0,sumfprpip=0.0,sumdelta=0.0;	
    SYNAPSE *syn,*syn2,*syn3;		
	
    net0->fitness = 0.0;
	        
    syn2=minwt.outputlist->synlist;
    syn3=maxwt.outputlist->synlist;

    for(syn=net0->outputlist->synlist;syn!=NULL;syn=syn->nextsyn){	

	syn2->weight = syn->weight;
	syn3->weight = syn->weight;

/*	printf("syn->weight %f\n",syn->weight);  */

    	sumdelta = 0.0;	
      	for(i=0;i<numoutput;i++){
		
/*	     printf("output %d\n",i);  */	

	     sumdeltao = 0.0;
    	     sumvp=0.0;sumerror=0.0;summult=0.0;

    	     for(j=0;j<numtrain;j++){

/*		printf("  train %d\n",j);    */	

		for(k=0;k<numinput;k++)input[k+1]=train_input[k][j];

        	nnet_propagate(best);
		nnet_propagate(net0);

        	error = train_output[i][j] - best->output[i]->out;
		sumerror += error; 
                sumvp += net0->output[i]->out;
		summult += error * net0->output[i]->out;
		fprime = net0->output[i]->out * (1-net0->output[i]->out);
		sumfprpipep += fprime * *syn->connect * error;
		sumfprpip += fprime * *syn->connect;
  
	    }

	    erroravg = sumerror/(float)numtrain;
  	    sumdeltao += sumfprpipep - erroravg * sumfprpip;

/*  	    printf("sumdeltao: %f\n",sumfprpipep - erroravg * sumfprpip);  */
	
  	}

	fitness = summult - sumvp*erroravg;	
	corrsign = (fitness<0) ? -1.0 : 1.0;
	sumdelta += corrsign * sumdeltao;	

/*	printf("connect %d sumdelta %f\n",syn->connect,sumdelta);   		

	printf("syn->weight+ga_tol*sumdelta %f\n",syn->weight+
		ga_tol*sumdelta);
*/
/*	if(syn2->weight > syn->weight+ga_tol*sumdelta)  */
		syn2->weight += ga_tol*sumdelta; 	

/*	if(syn3->weight < syn->weight+ga_tol*sumdelta)  */
		syn3->weight += ga_tol*sumdelta; 	

/*	syn->weight += sumdelta;
	net0->fitness += (fitness<0) ? (-fitness) : fitness; */
 
	syn2=syn2->nextsyn;	
	syn3=syn3->nextsyn;	
    }	

}


nnet_cc_disp_train(NNET *net0,NNET *best) 
/* apply training data to net and calculate fitness */
{
    int i,j,k;
    float sumvp=0.0,sumerror=0.0,summult=0.0,error,fitness=0.0;
        
    net0->fitness = 0.0;

    for(i=0;i<numoutput;i++){
    	sumvp=0.0;sumerror=0.0;summult=0.0;
    	for(j=0;j<numtrain;j++){
		for(k=0;k<numinput;k++)input[k+1]=train_input[k][j];
		printf("inputs: ");
		for(k=0;k<numinput+1;k++)
			printf("%f ",input[k]);;
                printf("\n");

        	nnet_propagate(best);
		nnet_propagate(net0);

        	error = train_output[i][j] - best->output[i]->out;
		printf("train_out %f best_out %f error %f\n",
			train_output[i][j],best->output[i]->out,error);

		sumerror += error; 
                sumvp += net0->output[i]->out;
		printf("candidate_out %f\n",net0->output[i]->out);
		summult += error*net0->output[i]->out;
	}

    	fitness = summult - sumvp*sumerror/(float)numtrain;	

        net0->fitness += (fitness<0) ? (-fitness) : fitness;

	printf("fitness: %f\n",net0->fitness);

    }	

}

rnd_seed()  /* seeds random number generator */
{

    int time_val;	

#ifdef VAX
    time(&time_val);
    rndseed = time_val;
#else
    printf("rndseed? ");
    scanf("%d%*c",&rndseed);
#endif
}

nnet_train_disp(NNET *net0) /* apply training data and calculate sse */
{
    int i,j;
    float error,sse=0.0;
        
    /* set up training data */

    printf("\nTraining run:\n");

    for(j=0;j<numtrain;j++){
	for(i=0;i<numinput;i++) input[i+1]=train_input[i][j];
	nnet_propagate(net0);
	printf("vector %2d:\n",j);
	for(i=0;i<numoutput;i++){
        	error = train_output[i][j] - net0->output[i]->out;
		sse += error * error;
		printf("            output #%2d: %6.2f expected: %6.2f\n",i,
			net0->output[i]->out,train_output[i][j]);
	}	
    }	
    net0->sse = sse;	
    printf("fitness : %6.2f sse: %6.2f\n",net0->fitness,sse);

}

fnnet_train_disp(NNET *net0) /* apply training data and calculate sse */
{
    int i,j,correct=0;
    float error,sse=0.0;

        
    /* set up training data */

    if((g = fopen("gacc.out","a")) == NULL) 
		printf("\007Error opening output file!\n");

   fprintf(g,"\nTraining run:\n");

    for(j=0;j<numtrain;j++){
	for(i=0;i<numinput;i++) input[i+1]=train_input[i][j];
	nnet_propagate(net0);
	fprintf(g,"vector %3d: ",j);
	printf("vector %3d: ",j);
	for(i=0;i<numoutput;i++){
        	error = train_output[i][j] - net0->output[i]->out;
		if(train_output[i][j]>=0.5 && net0->output[i]->out>=0.5){
			++correct;
			net0->output[i]->out=1.00;
		}
		else if(train_output[i][j]<0.5 && net0->output[i]->out<0.5){
			++correct;
			net0->output[i]->out=1.00;
		}
		else net0->output[i]->out=0.00;

		fprintf(g,"output #%2d: %6.2f expected: %6.2f\n",i,
			net0->output[i]->out,train_output[i][j]);
		printf("output #%2d: %6.2f expected: %6.2f\n",i,
			net0->output[i]->out,train_output[i][j]);
		sse += error * error;
	}	
    }	
    net0->sse = sse;	
    fprintf(g,"sse: %f\n",net0->sse);
    fprintf(g,"recognized %d / %d vectors\n",correct,numtrain);
    printf("sse: %f\n",net0->sse);
    printf("recognized %d / %d vectors\n",correct,numtrain);

    fclose(g);	

}

int fnnet_train_disp2(NNET *net0) /* apply training data and calculate sse */
{
    int i,j,correct=0;
    float error,sse=0.0;

        
    /* set up training data */

    fprintf(g,"Training run:\n");

    for(j=0;j<numtrain;j++){
	for(i=0;i<numinput;i++) input[i+1]=train_input[i][j];
	nnet_propagate(net0);
/*	fprintf(g,"vector %2d:\n",j); */
	for(i=0;i<numoutput;i++){
        	error = train_output[i][j] - net0->output[i]->out;
		if(train_output[i][j]>=0.5 && net0->output[i]->out>=0.5)
			++correct;
		if(train_output[i][j]<0.5 && net0->output[i]->out<0.5)
			++correct;
		sse += error * error;
/*		fprintf(g,"            output #%2d: %6.2f expected: %6.2f\n",i,
			net0->output[i]->out,train_output[i][j]);  */
	}	
    }	
    net0->sse = sse;	
/*    fprintf(g,"sse: %f\n",net0->sse); */
    fprintf(g,"recognized %d / %d vectors\n",correct,numtrain);
    printf("recognized %d / %d vectors\n",correct,numtrain);

    return(correct);	

}


binary_out(unsigned int j) 
{
    int i;
    unsigned int x=0x80;
    for(i=0;i<8;i++){
	if(x&j)printf("1");
	  else printf("0");
	if(i==3)printf("|");
	x >>= 1;
    }
}

int roulette_select(POP pop1,float sumfitness)
{

    float rndfitness,fitness=0.0;
    int i= -1;

    rndfitness = sumfitness * random();

    do{
    	++i;    
	fitness += pop1[i].fitness;      		
    }while(i<pop_size-1 && fitness<rndfitness);

    return(i);
}

copy_wt(NNET *net1, NNET *child)
{  /* 1/24/93 */

    int k,l;
    NEURON *neur1,*neur2;
    SYNAPSE *syn1,*syn2;

/*    for(neur1=net1->hiddenlist,neur2=child->hiddenlist;
		neur1!=NULL; neur1=neur1->nextneur,neur2=neur2->nextneur)
	for(syn1=neur1->synlist,syn2=neur2->synlist;
		syn1!=NULL; syn1=syn1->nextsyn,syn2=syn2->nextsyn)
           	     	syn1->weight = syn2->weight;	
*/

    for(neur1=net1->outputlist,neur2=child->outputlist;
		neur1!=NULL; neur1=neur1->nextneur,neur2=neur2->nextneur)
	for(syn1=neur1->synlist,syn2=neur2->synlist;
		syn1!=NULL; syn1=syn1->nextsyn,syn2=syn2->nextsyn)
           	     	syn1->weight = syn2->weight;	

    strncpy(net1->chromosome,child->chromosome,chromo_length);	
    
    net1->fitness = child->fitness;    
    net1->sse = child->sse;    

}


mutate(char *c)
{
    unsigned mask=0x01;
    int i;

    for(i=0;i<8;i++){
	if(random() < pmutate){
#ifdef DBCROSS	
	    printf("*");	
#endif
	    if(*c & mask) *c &= ~mask;
	    else *c |= mask;
	}
        mask <<= 1;
    }	
}    

swap(char *a, char *b)
{  /* swaps a and b */ 
    
    char temp;

    temp = *a;  
    *a = *b;
    *b = temp;

}    

bit_cross(char *child1,char *child2,int move)
{  

    unsigned int l,out1,out2;

    l=0xFF;
    l >>= move;
    l <<= move;    

    out1 = (l & *child2) | (~l & *child1);
    out2 = (l & *child1) | (~l & *child2);

#ifdef DBCROSS
    printf("bitcross: %d\n",move);
    binary_out(*child1);
    printf(": %d\n",*child1);
    binary_out(*child2);
    printf(": %d\n",*child2);
    binary_out(l);
    printf("\n");
    binary_out(~l);
    printf("\n");
    binary_out(out1);
    printf(": %d\n",out1);
    binary_out(out2);
    printf(": %d\n",out2);
#endif

    *child1 = out1;
    *child2 = out2;
} 


chromo_crossover(CHROMO *parent1,CHROMO *parent2,CHROMO *child1,CHROMO *child2)
{  /* performs crossover */

    int k,crossoversite,total=0;

/* if pcrossover, crossoversite = 1 to num_weights*bits_per_weight-1 */

    if(random()<pcrossover){     
	crossoversite = random()*(num_weights*bits_per_weight-1)+1;
    }
    else crossoversite = 0;	/* no crossover */

#ifdef DBCROSS
        printf("crossite: %3d mutate:",crossoversite);
#endif
    
/*    printf("length %d crossoversite: %d\n",num_weights*bits_per_weight,
	crossoversite);  
*/    
    for(k=0;k<chromo_length;k++){

	if(k<crossoversite){
		child1[k] = parent2[k];
		child2[k] = parent1[k];
	}

	else if(k>=crossoversite && k<crossoversite+8){    
		child1[k] = parent1[k];
		child2[k] = parent2[k];
		bit_cross(&child1[k],&child2[k],k-crossoversite);
	}

	else if(k>crossoversite){    
		child1[k] = parent1[k];
		child2[k] = parent2[k];
	}

	mutate(&child1[k]);
	mutate(&child2[k]);

    }	

#ifdef DBCROSS	
    printf("\n");
#endif

}    

pop_histogram(POP pop1)
{

    	int histo[21],temp,i,j;
	
	for(i=0;i<pop_size;i++){
		++ histo[(int)pop1[i].fitness];
 	}
	
        histo[19] += histo[20];

	for(i=0;i<20;i++){
		printf("%3.1f - %3.1f ",(float)i,(float)i+1);
		printf(" %3d",histo[i]);
		printf("\n");
	}

}

/* The basic routine for doing quickprop-style update of weights, given a
 * pair of slopes and a delta.
 *
 * Given arrays holding weights, deltas, slopes, and previous slopes,
 * and an index i, update weight[i] and delta[i] appropriately.  Move
 * slope[i] to prev[i] and zero out slope[i].  Add weight decay term to
 * each slope before doing the update.
 */
void quickprop_update(SYNAPSE *syn, 
		      float epsilon, float decay, float mu, 
		      float shrink_factor)
{
  float w,d,s,p, next_step;
  /********/

  w = syn->weight;
  d = syn->deltas;
  s = syn->slopes +  decay * w;
  p = syn->prevslopes;

  next_step = 0.0;

  /* The step must always be in direction opposite to the slope. */

  if(d < 0.0){			
    /* If last step was negative...  */  
    if(s > 0.0)	  
      /*  Add in linear term if current slope is still positive.*/
      next_step -= epsilon * s;
    /*If current slope is close to or larger than prev slope...  */
    if(s >= (shrink_factor*p)) 
      next_step += mu * d;	/* Take maximum size negative step. */
    else
      next_step += d * s / (p - s); /* Else, use quadratic estimate. */
  }
  else if(d > 0.0){
    /* If last step was positive...  */
    if(s < 0.0)	  
      /*  Add in linear term if current slope is still negative.*/
      next_step -= epsilon * s;
    /* If current slope is close to or more neg than prev slope... */
    if(s <= (shrink_factor*p)) 
      next_step += mu * d;	/* Take maximum size negative step. */
    else
      next_step += d * s / (p - s); /* Else, use quadratic estimate. */
  }
  else
    /* Last step was zero, so use only linear term. */
    next_step -= epsilon * s;
  
  /* update global data arrays */
  syn->deltas = next_step;
  syn->weight = w + next_step;
  syn->prevslopes = s;
  syn->slopes = 0.0;
}

nnet_cc_quickprop(NNET *net0,NNET *best) 
/* apply training data to candidate and modify weights */
{

    int i,j,k;
    float sumvp=0.0,sumerror=0.0,summult=0.0,error,fitness=0.0,sumdeltao=0.0;
    float fprime,corrsign,erroravg,sumfprpipep=0.0,sumfprpip=0.0,sumdelta=0.0;	
    SYNAPSE *syn;		
	
    net0->fitness = 0.0;
	        
    for(syn=net0->outputlist->synlist;syn!=NULL;syn=syn->nextsyn){	

/*	printf("syn->weight %f\n",syn->weight);  */

    	sumdelta = 0.0;	
      	for(i=0;i<numoutput;i++){
		
/*	     printf("output %d\n",i);  */	

	     sumdeltao = 0.0;
    	     sumvp=0.0;sumerror=0.0;summult=0.0;

    	     for(j=0;j<numtrain;j++){

/*		printf("  train %d\n",j);    */	

		for(k=0;k<numinput;k++)input[k+1]=train_input[k][j];

        	nnet_propagate(best);
		nnet_propagate(net0);

        	error = train_output[i][j] - best->output[i]->out;
		sumerror += error; 
                sumvp += net0->output[i]->out;
		summult += error * net0->output[i]->out;
		fprime = net0->output[i]->out * (1-net0->output[i]->out);
		sumfprpipep += fprime * *syn->connect * error;
		sumfprpip += fprime * *syn->connect;
  
	    }

	    erroravg = sumerror/(float)numtrain;
  	    sumdeltao += sumfprpipep - erroravg * sumfprpip;

/*  	    printf("sumdeltao: %f\n",sumfprpipep - erroravg * sumfprpip);  */
	
  	}

	fitness = summult - sumvp*erroravg;	
	corrsign = (fitness<0) ? -1.0 : 1.0;
	sumdelta += corrsign * sumdeltao;	

/*	printf("connect %d sumdelta %f\n",syn->connect,sumdelta);   		

	printf("syn->weight+ga_tol*sumdelta %f\n",syn->weight+
		ga_tol*sumdelta);
*/

/*	syn->weight += sumdelta;   */
	syn->slopes -= sumdelta;

	net0->fitness += (fitness<0) ? (-fitness) : fitness; 
 
    }	

	
	for(i=0;i<numoutput;i++)
	   for(syn=net0->output[i]->synlist;syn!=NULL;syn = syn->nextsyn)	
	      quickprop_update(syn,1.0/(float)(numtrain*(numhidden+numinput)),
		0.0,2.0,0.666666);	

	nnet_cc_train(net0,best);

/*	printf("Quickprop fitness: %f\n",net0->fitness);   */

}

nnet_quickprop(NNET *net0)  /* 3/15/93 */
{

    int i,j,k,l,m;
    float error,sse=0.0,adj,deriv,grad,maxgrad=0.0,mingrad=0.0,tempweight;   
    NEURON *neur;
    SYNAPSE *syn,*syn2,*syn3;		
	
    for(i=0;i<numoutput;i++){
/*    	printf("output %d\n",i); */	
    	for(syn=net0->output[i]->synlist;syn!=NULL;syn = syn->nextsyn){	
		syn->slopes = 0.0;	
/*		printf("syn: weight %f slopes %f deltas %f prevslopes %f\n",
		syn->weight,syn->slopes,syn->deltas,syn->prevslopes);  */
        }
     }

    for(j=0;j<numtrain;j++){

	for(i=0;i<numinput;i++) input[i+1]=train_input[i][j];
	nnet_propagate(net0);

	/* for output layer */	
	for(i=0;i<numoutput;i++){
         	error = net0->output[i]->out - train_output[i][j] ;
		sse += error * error;
		deriv = 0.1 + net0->output[i]->out * (1-net0->output[i]->out);  
		net0->output[i]->delta = error * deriv;
	}
 
	for(i=0;i<numoutput;i++){
		for(syn=net0->output[i]->synlist;syn!=NULL;syn = syn->nextsyn){	
		    grad=net0->output[i]->delta * *syn->connect;
                    syn->slopes += grad;
		    adj =learning_rate * grad;

		}
	}


    }


	for(i=0;i<numoutput;i++)
	   for(syn=net0->output[i]->synlist;syn!=NULL;syn = syn->nextsyn)	
	      quickprop_update(syn,0.35/(float)numtrain,0.0001,2.0,0.666666);	

    sse = 0;	

    for(j=0;j<numtrain;j++){
	for(i=0;i<numinput;i++) input[i+1]=train_input[i][j];
    	nnet_propagate(net0);	
    	for(i=0;i<numoutput;i++){
         	error = train_output[i][j] - net0->output[i]->out;
	 	sse += error * error;
    	}
    }	

/*    printf("Quickprop: sse = %f\n",sse); */	

    net0->sse = sse;	

    net0->fitness = 4*numtrain*numoutput - sse;	

}

nnet_backprop(NNET *net0)
{

    int i,j,k,l,m;
    float error,sse=0.0,adj,deriv,grad,maxgrad=0.0,mingrad=0.0,tempweight;   
    NEURON *neur;
    SYNAPSE *syn,*syn2,*syn3;		
	
    for(j=0;j<1;j++){

	for(i=0;i<numinput;i++) input[i+1]=train_input[i][j];
	nnet_propagate(net0);

	/* for output layer */	
	for(i=0;i<numoutput;i++){
         	error = train_output[i][j] - net0->output[i]->out;
		sse += error * error;
		deriv = net0->output[i]->out * (1-net0->output[i]->out);  
		net0->output[i]->delta = error * deriv;
	}

	for(i=0;i<numoutput;i++){
/*		printf("output %d\n",i);        */
                syn2=minwt.output[i]->synlist;
                syn3=maxwt.output[i]->synlist;
		for(syn=net0->output[i]->synlist;syn!=NULL;syn = syn->nextsyn){	
		    grad=net0->output[i]->delta * *syn->connect;
		    adj =learning_rate * grad;
/*		    printf("connect %d gradient: %f\n",syn,grad);     */	
		    syn->prevdelta = adj + momentum * syn->prevdelta;	
			syn3->weight = syn->weight+ga_tol*adj; 	
			syn2->weight = syn->weight+ga_tol*adj; 	
		    syn2=syn2->nextsyn;	
		    syn3=syn3->nextsyn;	
		}
	}


    }

    for(j=1;j<numtrain;j++){

	for(i=0;i<numinput;i++) input[i+1]=train_input[i][j];
	nnet_propagate(net0);

	/* for output layer */	
	for(i=0;i<numoutput;i++){
         	error = train_output[i][j] - net0->output[i]->out;
		sse += error * error;
		deriv = net0->output[i]->out * (1-net0->output[i]->out);  
		net0->output[i]->delta = error * deriv;
	}

	for(i=0;i<numoutput;i++){
/*		printf("output %d\n",i);      */
                syn2=minwt.output[i]->synlist;
                syn3=maxwt.output[i]->synlist;
		for(syn=net0->output[i]->synlist;syn!=NULL;syn = syn->nextsyn){	
		    grad=net0->output[i]->delta * *syn->connect;
		    adj =learning_rate * grad;
/*		    printf("connect %d gradient: %f\n",syn,grad);    */	
		    syn->prevdelta = adj + momentum * syn->prevdelta;	
		    if(syn->weight+ga_tol*adj > syn3->weight)
			syn3->weight = syn->weight+ga_tol*adj; 	
		    if(syn->weight+ga_tol*adj < syn2->weight)
			syn2->weight = syn->weight+ga_tol*adj; 	
		    syn2=syn2->nextsyn;	
		    syn3=syn3->nextsyn;	
		}
	}


    }

/*    printf("maxgrad %f mingrad %f\n",maxgrad,mingrad);   */	

/*    max_weight = tempweight + maxgrad; */

/*    min_weight = tempweight + mingrad; */
	
    /* evaluate network */	
    sse = 0.0;	

    for(j=0;j<numtrain;j++){
	for(i=0;i<numinput;i++) input[i+1]=train_input[i][j];
    	nnet_propagate(net0);	
    	for(i=0;i<numoutput;i++){
         	error = train_output[i][j] - net0->output[i]->out;
	 	sse += error * error;
    	}
    }	

/*    printf("Backprop: sse = %f\n",sse);  */	

    net0->sse = sse;	

    net0->fitness = 4*numtrain*numoutput - sse;	

}

nnet_backprop2(NNET *net0)
{

/* tried to use pointers instead of outputlist array, doesn't work */

    int i,j,k,l,m;
    float error,sse=0.0,adj,deriv,grad,maxgrad=0.0,mingrad=0.0,tempweight;   
    NEURON *neur;
    SYNAPSE *syn,*syn2,*syn3;		
	
    for(j=0;j<1;j++){

	for(i=0;i<numinput;i++) input[i+1]=train_input[i][j];
	nnet_propagate(net0);

	/* for output layer */	
	for(i=0,neur=net0->outputlist;i<numoutput;i++,neur=neur->nextneur){
         	error = train_output[i][j] - neur->out;
		sse += error * error;
		deriv = neur->out * (1-neur->out);  
		neur->delta = error * deriv;
	}

	for(i=0,neur=net0->outputlist;i<numoutput;i++,neur=neur->nextneur){
/*		printf("output %d\n",i);        */
                syn2=minwt.output[i]->synlist;
                syn3=maxwt.output[i]->synlist;
		for(syn=net0->output[i]->synlist;syn!=NULL;syn = syn->nextsyn){	
		    grad=net0->output[i]->delta * *syn->connect;
		    adj =learning_rate * grad;
/*		    printf("connect %d gradient: %f\n",syn,grad);     */	
		    syn->prevdelta = adj + momentum * syn->prevdelta;	
			syn3->weight = syn->weight+ga_tol*adj; 	
			syn2->weight = syn->weight+ga_tol*adj; 	
		    syn2=syn2->nextsyn;	
		    syn3=syn3->nextsyn;	
		}
	}


    }

    for(j=1;j<numtrain;j++){

	for(i=0;i<numinput;i++) input[i+1]=train_input[i][j];
	nnet_propagate(net0);

	/* for output layer */	
	for(i=0;i<numoutput;i++){
         	error = train_output[i][j] - net0->output[i]->out;
		sse += error * error;
		deriv = net0->output[i]->out * (1-net0->output[i]->out);  
		net0->output[i]->delta = error * deriv;
	}

	for(i=0;i<numoutput;i++){
/*		printf("output %d\n",i);      */
                syn2=minwt.output[i]->synlist;
                syn3=maxwt.output[i]->synlist;
		for(syn=net0->output[i]->synlist;syn!=NULL;syn = syn->nextsyn){	
		    grad=net0->output[i]->delta * *syn->connect;
		    adj =learning_rate * grad;
/*		    printf("connect %d gradient: %f\n",syn,grad);    */	
		    syn->prevdelta = adj + momentum * syn->prevdelta;	
		    if(syn->weight+ga_tol*adj > syn3->weight)
			syn3->weight = syn->weight+ga_tol*adj; 	
		    if(syn->weight+ga_tol*adj < syn2->weight)
			syn2->weight = syn->weight+ga_tol*adj; 	
		    syn2=syn2->nextsyn;	
		    syn3=syn3->nextsyn;	
		}
	}


    }

/*    printf("maxgrad %f mingrad %f\n",maxgrad,mingrad);   */	

/*    max_weight = tempweight + maxgrad; */

/*    min_weight = tempweight + mingrad;  */
	
    /* evaluate network */	
    sse = 0.0;	

    for(j=0;j<numtrain;j++){
	for(i=0;i<numinput;i++) input[i+1]=train_input[i][j];
    	nnet_propagate(net0);	
    	for(i=0;i<numoutput;i++){
         	error = train_output[i][j] - net0->output[i]->out;
	 	sse += error * error;
    	}
    }	

    net0->sse = sse;	

    net0->fitness = 4*numtrain*numoutput - sse;	

}


nnet_disp_backprop(NNET *net0)
{

    int i,j,k,l,m;
    float error,adj,deriv,sumerror=0.0;		
    NEURON *neur;
    SYNAPSE *syn,*syn2;		

    for(j=0;j<numtrain;j++){
	for(i=0;i<numinput;i++) input[i+1]=train_input[i][j];
	nnet_propagate(net0);
    	printf("Training vector #%d\n",j);	
	printf("Find deltas:\n");
	/* for output layer */	
	for(i=0;i<numoutput;i++){
        	error = train_output[i][j] - net0->output[i]->out;
		sumerror += error * error;
		deriv = net0->output[i]->out * (1-net0->output[i]->out);  
		net0->output[i]->delta = error * deriv;
		printf("  output node %d output %f desired %f error %f\n",
			net0->output[i],net0->output[i]->out,
			train_output[i][j],error);
		printf("    f'(net) %f delta %f\n",deriv,
			net0->output[i]->delta);
	}
	/* for middle layer */	
	for(i=0;i<nummiddle;i++){
		deriv = net0->middle[i]->out * (1-net0->middle[i]->out);  
		printf("  middle node %d output %f f'(net) %f\n",
			net0->middle[i],net0->middle[i]->out,deriv);
		for(error=0,l=0;l<numoutput;l++){
			for(syn2=net0->output[l]->synlist,k=0;k<i+1;
				k++,syn2 = syn2->nextsyn);	
                    	error += net0->output[l]->delta * syn2->weight;
                    printf("    output node %d delta %f weight %f sdelta %f\n",
		      	net0->output[l],net0->output[l]->delta,syn2->weight,
			error);
		}		
		net0->middle[i]->delta = error * deriv;		    	
		printf("  total middle delta %f\n",net0->middle[i]->delta);
	}
	printf("Adjust weights:\n");
	for(i=0;i<numoutput;i++){
        	for(syn=net0->output[i]->synlist;syn!=NULL;syn = syn->nextsyn){	
		    adj =learning_rate * net0->output[i]->delta * *syn->connect;
    		    printf("  output neuron %d delta %f\n",net0->output[i],
			net0->output[i]->delta); 	
    		    printf("    middle %d output %f\n",syn->connect,
			*syn->connect);	
		    printf("    weight %f adj %f\n",syn->weight,adj);	
   	            syn->weight += adj;
		}
	}
	for(i=0;i<nummiddle;i++){
		for(syn=net0->middle[i]->synlist;syn!=NULL;syn=syn->nextsyn){
		    adj =learning_rate * net0->middle[i]->delta * *syn->connect;
    		    printf("  middle neuron %d delta %f\n",net0->middle[i],
			net0->middle[i]->delta); 	
    		    printf("    input %d output %f\n",syn->connect,
			*syn->connect);	
		    printf("    weight %f adj %f\n",syn->weight,adj);	
   	            syn->weight += adj;
		}
	}

    nnet_propagate(net0);	

    }

    printf("SSE: %f\n",sumerror);	
}

POP pop_create()
/* create population */
{
    int i;	
    POP pop1;	

    pop1 = net_alloc(pop_size);
    for(i=0;i<pop_size;i++){
        nnet_cc_create(&pop1[i]);
    }
    return(pop1);	
}

POP pop_create_candidate(NNET *best)
/* create population */
{
    int i;	
    POP pop1;	

    pop1 = net_alloc(pop_size);
    for(i=0;i<pop_size;i++){
        nnet_cc_candidate(&pop1[i],best);
    }
    return(pop1);	
}

pop_fitness2(POP pop1)
/* figure fitness  1/23/93 */
{

    int c,k;

    c = 4*numtrain*numoutput;		

    for(k=0;k<pop_size;k++){
	pop1[k].fitness = c - pop1[k].sse;	
    }

}

pop_train(POP pop1)
/* train population */
{
    int i;	

    for(i=0;i<pop_size;i++){
	nnet_train(&pop1[i]);
    }
}

pop_train2(POP pop1,NNET *best)
/* train population 1/23/93 */
{

    int i,j,k;
    float error;
    NEURON *neur;	
        
    for(k=0;k<pop_size;k++){
	pop1[k].sse=0.0;
    }

    /* set up training data */

    for(j=0;j<numtrain;j++){

	for(i=0;i<numinput;i++) 
		input[i+1]=train_input[i][j];

	nnet_propagate_hidden(best);  

    	for(k=0;k<pop_size;k++){

		nnet_propagate(&pop1[k]);

		for(i=0,neur=pop1[k].outputlist;i<numoutput;
		  i++,neur=neur->nextneur){
        		error = train_output[i][j] - neur->out;
			pop1[k].sse += error * error; 
		}
    	}	

    }

}

float pop_sumfitness(POP pop1)
/* sum fitness for population */
{
    int i;	
    float sumfitness;	

    for(i=0,sumfitness=0.0;i<pop_size;i++){
	sumfitness += pop1[i].fitness;
    }

    return(sumfitness);	
}

int pop_bestof(POP pop1)
/* return bestfit of population */
{
    int i,bestfitness;	

    for(i=0,bestfitness=0;i<pop_size;i++){
	if(pop1[i].fitness > pop1[bestfitness].fitness) bestfitness=i;
    }

    return(bestfitness);	
}

int pop_worstof(POP net0)
{
    int i,j,worst;

    worst = random()*pop_size;

    for(i=2;i< 10 ;i++){
	j = random()* pop_size;
	if(net0[j].fitness < net0[worst].fitness) worst = j; 
    }

    return(worst);
}

pop_swap(POP *pop1,POP *pop2)
/* 1/25/93 */
{
	POP temp;

        temp = *pop1;
	*pop1 = *pop2;
	*pop2 = temp;

}
	
pop_non_overlapping2(POP pop1,POP pop2,float sumfitness)
/* 1/23/93 */
{

	int i,parent1,parent2;

/*	printf("POP TEST\n");

	printf("pop1\n");
	pop_disp(pop1);

	printf("pop2\n");
        pop_disp(pop2);

	printf("CROSSOVER\n");
*/
	for(i=0;i<pop_size;i+=2){	

	    parent1 = roulette_select(pop1,sumfitness);
	    parent2 = roulette_select(pop1,sumfitness);
#ifdef DBCROSS
	    printf("parents: %3d %3d ",parent1,parent2);	
#endif
	    chromo_crossover(pop1[parent1].chromosome,
		pop1[parent2].chromosome,pop2[i].chromosome,
		pop2[i+1].chromosome);

	    nnet_wt_from_chromo(&pop2[i]);
	    nnet_wt_from_chromo(&pop2[i+1]);
	
	}

  /*	printf("pop1\n");
	pop_disp(pop1);

	printf("pop2\n");
        pop_disp(pop2);
*/


}

pop_can_non_overlapping(POP pop1,POP pop2,float sumfitness,NNET *best)
{

	int i,parent1,parent2;

	for(i=0;i<pop_size;i+=2){	

	    parent1 = roulette_select(pop1,sumfitness);
	    parent2 = roulette_select(pop1,sumfitness);

	    chromo_crossover(pop1[parent1].chromosome,
		pop1[parent2].chromosome,pop2[i].chromosome,
		pop2[i+1].chromosome);

	    nnet_wt_from_chromo(&pop2[i]);
	    nnet_wt_from_chromo(&pop2[i+1]);
	
	    nnet_cc_train(&pop2[i],best);
	    nnet_cc_train(&pop2[i+1],best);

	}

        pop_swap(&pop1,&pop2);

}

pop_bin_disp(POP pop1)	
{

    int i;	
	
    for(i=0;i<pop_size;i++){
	printf("%3d ",i);
	nnet_bin_disp(&pop1[i]);
    }		
}

nnet_bitswitch(NNET *net0) /* one-at-a-time mutation */
{
                                                            
    int i,j,k,l,bestbyte= -1,bestbit;
    float bestfit;		

    bestfit = net0->fitness;

/*    printf("bitswitch\n");	
    nnet_disp(net0);	
    printf("Start fit: %f\n",bestfit);  	
    		chromo_disp(net0->chromosome);
        	k = bit_decode(net0->chromosome[0],0);
		bit_encode(&net0->chromosome[0],0,k);
    		chromo_disp(net0->chromosome);
		nnet_wt_from_chromo(net0);
		nnet_train(net0);
    nnet_disp(net0);	
    printf("Start fit: %f\n",bestfit); */ 	
	
    for(j=0;j<chromo_length;j++){
	for(i=0;i<8;i++){
        	k = bit_decode(net0->chromosome[j],i);
		if(k==0) l=1;
                else l=0;		
		bit_encode(&net0->chromosome[j],i,l);
		nnet_wt_from_chromo(net0);
		nnet_train(net0);
		if(net0->fitness > bestfit){
			bestbyte = j;
			bestbit  = i;
			bestfit = net0->fitness;
/*			printf("bestbyte %d\n",j);*/
		}
/*    		chromo_disp(net0->chromosome);    */
/*                printf("  new fitness: %f\n",net0->fitness); */
		bit_encode(&net0->chromosome[j],i,k);
        }
    }  	

    if(bestbyte != -1){	
    	k = bit_decode(net0->chromosome[bestbyte],bestbit);
    	if(k==0) l=1;
    	else l=0;		
    	bit_encode(&net0->chromosome[bestbyte],bestbit,l);
    }

/*    printf("arrgggghhh\n");
    chromo_disp(net0->chromosome);   */
    nnet_wt_from_chromo(net0);
    nnet_train(net0);
/*    nnet_disp(net0); */	
/*    printf("arrgggghhh end of bitswitch\n"); */
	

}

nnet_cc_bitswitch(NNET *net0,NNET *best)
/* one-at-a-time mutation for candidates */
{
                                                            
    int i,j,k,l,bestbyte= -1,bestbit;
    float bestfit;		

    bestfit = net0->fitness;

/*    printf("bitswitch\n");	
    nnet_disp(net0);	
    printf("Start fit: %f\n",bestfit);  	
    		chromo_disp(net0->chromosome);
        	k = bit_decode(net0->chromosome[0],0);
		bit_encode(&net0->chromosome[0],0,k);
    		chromo_disp(net0->chromosome);
		nnet_wt_from_chromo(net0);
		nnet_cc_train(net0,best);
    nnet_disp(net0);	
    printf("Start fit: %f\n",bestfit); */ 	
	
    for(j=0;j<chromo_length;j++){
	for(i=0;i<8;i++){
        	k = bit_decode(net0->chromosome[j],i);
		if(k==0) l=1;
                else l=0;		
		bit_encode(&net0->chromosome[j],i,l);
		nnet_wt_from_chromo(net0);
		nnet_cc_train(net0,best);
		if(net0->fitness > bestfit){
			bestbyte = j;
			bestbit  = i;
			bestfit = net0->fitness;
/*			printf("bestbyte %d\n",j);*/
		}
/*    		chromo_disp(net0->chromosome);    */
/*                printf("  new fitness: %f\n",net0->fitness); */
		bit_encode(&net0->chromosome[j],i,k);
        }
    }  	

    if(bestbyte != -1){	
    	k = bit_decode(net0->chromosome[bestbyte],bestbit);
    	if(k==0) l=1;
    	else l=0;		
    	bit_encode(&net0->chromosome[bestbyte],bestbit,l);
    }

/*    printf("arrgggghhh\n");
    chromo_disp(net0->chromosome);   */
    nnet_wt_from_chromo(net0);
    nnet_cc_train(net0,best);
/*    nnet_disp(net0); */	
/*    printf("arrgggghhh end of bitswitch\n"); */
	

}


main() 
{
    char h,reply;
    int i,j,k,l,w,z,bestfitness,rdm,worst,count,flag=0;
    int time_val,match,maxmatch,parenttmp,correct;
    int stag;
    float stagsse;	

    NNET *temp,*child1,*child2,best,bestcan,best2,bestcan2;
    POP pop1,pop2,pop3,pop4;	 
    float sumfitness,pat_fitness,bestswitch,bestsofar;	
    NEURON *neur;
    SYNAPSE *syn;		

    signal(SIGINT,quit);
    signal(SIGTERM,quit);
    signal(SIGQUIT,quit);
	
    rnd_seed();
    load_training();
    begin_stats();	

    nnet_create_noc(&minwt);	
    nnet_create_noc(&maxwt);	

    for(neur=minwt.outputlist;neur!=NULL;neur=neur->nextneur) 
    	for(syn=neur->synlist;syn!=NULL;syn=syn->nextsyn) 
    		syn->weight=min_weight;

    for(neur=maxwt.outputlist;neur!=NULL;neur=neur->nextneur) 
    	for(syn=neur->synlist;syn!=NULL;syn=syn->nextsyn) 
    		syn->weight=max_weight;

/*    nnet_disp(&minwt);
    nnet_disp(&maxwt);
*/   

    pop1 = pop_create();
/*    printf("pop1\n");	
    nnet_disp(&pop1[0]);	
    nnet_disp(&pop1[1]);	
    printf("pop2\n");	
*/ 

   pop2 = pop_create();

/*    nnet_disp(&pop2[0]);	
    nnet_disp(&pop2[1]);	
*/

    pop3 = pop_create_candidate(&best);	
    pop4 = pop_create_candidate(&best);	

    nnet_cc_create(&best);

    nnet_cc_candidate(&bestcan,&best);	

    nnet_cc_create(&best2);

    nnet_cc_candidate(&bestcan2,&best);	

/*    printf("best\n");
    nnet_disp(&best);
    printf("bestcan\n");
    nnet_disp(&bestcan);	
*/  	

    for(;;){
	
/*    	printf("pop1\n");
    	pop_disp(pop1);
    	printf("pop2\n");
    	pop_disp(pop2);	
    	printf("pop3\n");
    	pop_disp(pop3);
    	printf("pop4\n");
    	pop_disp(pop4);
*/

	/* TRAINING OUTPUT WEIGHTS */

	flag = 0;

    	printf("\nTraining output weights\n");	

	min_weight = -10.0;
        max_weight = 10.0;

        for(neur=minwt.outputlist;neur!=NULL;neur=neur->nextneur) 
    	    for(syn=neur->synlist;syn!=NULL;syn=syn->nextsyn) 
    		syn->weight=min_weight;

        for(neur=maxwt.outputlist;neur!=NULL;neur=neur->nextneur) 
    	    for(syn=neur->synlist;syn!=NULL;syn=syn->nextsyn) 
    		syn->weight=max_weight;

	/* OUTPUT WEIGHTS LOOP */
				   
	ga_tol = 10.0;
	count = 0;

 	for(z=0;z<1;z++){       

	    	    						
	    for(i=0;i<pop_size;i++){	
	    	chromo_random(pop1[i].chromosome);
	    	nnet_wt_from_chromo(&pop1[i]);
            }	

	    pop_train2(pop1,&best);
	    pop_fitness2(pop1);	

	    sumfitness = pop_sumfitness(pop1);
	    bestfitness = pop_bestof(pop1);

    	    if(pop1[bestfitness].fitness > best.fitness){  
	    	copy_wt(&best,&pop1[bestfitness]);
                count = 0;
/*                printf("best: %f bestfitness: %f\n",best.fitness,
			pop1[bestfitness].fitness);   */
    	    }	  	

	    if(!flag){
		copy_wt(&best,&pop1[bestfitness]);
                count = 0;
		flag = 1;
 	    }

/*    	    printf("Iter: %5d Best: %8.5f Avg: %8.5f\n",j=0,
	    pop1[bestfitness].sse,4*numtrain*numoutput-sumfitness/pop_size);	
*/
    	    for(j=1;j<20;j++){   
				    			
/*               printf("BEFORE POP_NON\n");
		printf("pop1\n");
   	 	pop_disp(pop1);
    		printf("pop2\n");
    		pop_disp(pop2);	
*/
		pop_non_overlapping2(pop1,pop2,sumfitness);
        	pop_swap(&pop1,&pop2);
		
		pop_train2(pop1,&best);
		pop_fitness2(pop1);	
	
/*              printf("AFTER POP_NON\n");  
		printf("pop1\n");
   	 	pop_disp(pop1);
    		printf("pop2\n");
    		pop_disp(pop2);	
*/

		sumfitness = pop_sumfitness(pop1);
    		bestfitness = pop_bestof(pop1);

    	 	if(pop1[bestfitness].fitness > best.fitness){ 
		    copy_wt(&best,&pop1[bestfitness]);
		    count = 0;
		}	
                else{ 
			if(ga_tol<10.0){  
				count++;
				printf("Thats %d...\n",count);
			}
	        }

/*	        if(!(j%iterations))    */
		    printf("Iter: %5d Best: %8.5f Avg: %8.5f\n",j,
			pop1[bestfitness].sse,
	                4*numtrain*numoutput-sumfitness/pop_size);	

  /*              printf("pop1\n");
                pop_disp(pop1);
                printf("pop2\n");
                pop_disp(pop2);
*/
	    	if(ga_tol<9.0 && j==1){
			break;
		 	pcrossover=0.0;
		 	pmutate=0.5;
	    	}			

	    }

/*	    printf("ga_tol = %f\n",ga_tol);  */	
                                                       
/*	    printf("Best\n");	
	    nnet_disp(&best);
*/
	    nnet_backprop(&best);	

	    ga_tol = 1.0;	

	    if(count==5) break;	

	}

	printf("\nBEST WITH %d HIDDEN NODES:\n",numhidden);	


/*    	if((g = fopen("gacc.out","a")) == NULL) 
		printf("\007Error opening output file!\n");

	correct = fnnet_train_disp2(&best);

	fprintf(g,"hidden nodes: %d fitness: %f sse: %f\n",numhidden,
	    best.fitness, best.sse);  
	
        fnnet_disp(&best);

	fclose(g);	
*/
/*        nnet_disp(&best);   */

    for(i=0;i<numoutput;i++){
    	for(syn=best.output[i]->synlist;syn!=NULL;syn = syn->nextsyn){	
		syn->slopes = 0.0;	
		syn->deltas = 0.0;	
		syn->prevslopes = 0.0;	
/*		printf("syn: weight %f slopes %f deltas %f prevslopes %f\n",
		syn->weight,syn->slopes,syn->deltas,syn->prevslopes);  */
        }
     }

	stag=0;
	stagsse=best.sse;	

	for(i=0;i<1000;i++){
		nnet_quickprop(&best);
		if(!(i%100)) printf("Quickprop iter %d best sse: %f\n",
			i,best.sse);
 		if(stagsse>best.sse){
		   stagsse=best.sse;
	           stag = 0;	
		   copy_wt(&best2,&best);	
		}
		else stag++;
		if(stag>20) break;
	}

	copy_wt(&best,&best2);	

	printf("best after %d Quickprop iters: %f\n", i-stag, best.sse);

/*	nnet_disp(&best);
*/
    	if((g = fopen("gacc.out","a")) == NULL) 
		printf("\007Error opening output file!\n");

	correct = fnnet_train_disp2(&best);

	fprintf(g,"hidden nodes: %d fitness: %f sse: %f\n",numhidden,
	    best.fitness, best.sse);  
	
        fnnet_disp(&best);

	fclose(g);	

        if(numhidden == nummiddle || correct == numtrain){  
		 nnet_test(&best);
		 break;
	}	

	/* TRAINING HIDDEN WEIGHTS */

	printf("\nTraining hidden weights\n");	

	min_weight = -20.0;  	  

    	for(neur=minwt.outputlist;neur!=NULL;neur=neur->nextneur) 
    		for(syn=neur->synlist;syn!=NULL;syn=syn->nextsyn) 
    			syn->weight=min_weight;

	max_weight = 20.0;  	

    	for(neur=maxwt.outputlist;neur!=NULL;neur=neur->nextneur) 
    		for(syn=neur->synlist;syn!=NULL;syn=syn->nextsyn) 
    			syn->weight=max_weight;

	/*  CANDIDATE LOOP  */

	for(ga_tol = 1000.0;ga_tol>/*1.0*/100.0;ga_tol/=10.0){   

		for(i=0;i<pop_size;i++){
	    		chromo_random(pop3[i].chromosome);
	    		nnet_wt_from_chromo(&pop3[i]);
	    		nnet_cc_train(&pop3[i],&best);
		}	

/*        	printf("pop3\n"); pop_disp(pop3);   */

		sumfitness = pop_sumfitness(pop3);
		bestfitness = pop_bestof(pop3);

		copy_wt(&bestcan,&pop3[bestfitness]);	

/*	    	printf("Iter: %5d Best: %8.5f Avg: %8.5f\n",j=0,
	        	bestcan.fitness,sumfitness/pop_size);	
*/
	    	for(j=1;j<20;j++){

	/*    		printf("Before nonoverlap\n");
    			printf("pop3\n");
    			pop_disp(pop3);
    			printf("pop4\n");
    			pop_disp(pop4);  
        */
			pop_non_overlapping2(pop3,pop4,sumfitness);
        		pop_swap(&pop3,&pop4);

			for(i=0;i<pop_size;i++){
				nnet_cc_train(&pop3[i],&best);
                	}

    			sumfitness = pop_sumfitness(pop3);
    			bestfitness = pop_bestof(pop3);

	/*    		printf("After nonoverlap\n");
    			printf("pop3\n");
    			pop_disp(pop3);
    			printf("pop4\n");
    			pop_disp(pop4);
	*/
    			if(pop3[bestfitness].fitness > bestcan.fitness) 
				copy_wt(&bestcan,&pop3[bestfitness]);	

/*			if(!(j%iterations))  */	
			printf("Iter: %5d Best: %8.5f Avg: %8.5f\n",j,
	    			pop3[bestfitness].fitness,sumfitness/pop_size);	

	        }

/*	    	printf("BEST CANDIDATE:\n");	
	    	nnet_disp(&bestcan);
*/	    	printf("Best correlation: %f\n",bestcan.fitness);  

   /*	    	nnet_cc_update(&bestcan,&best);    */
	
/*	    	printf("ga_tol = %f\n",ga_tol);	  */

	}

    for(i=0;i<numoutput;i++){
    	for(syn=bestcan.output[i]->synlist;syn!=NULL;syn = syn->nextsyn){	
		syn->slopes = 0.0;	
		syn->deltas = 0.0;	
		syn->prevslopes = 0.0;	
/*		printf("syn: weight %f slopes %f deltas %f prevslopes %f\n",
		syn->weight,syn->slopes,syn->deltas,syn->prevslopes);  */
        }
     }

	stag=0;
	stagsse=bestcan.fitness;	

	for(i=0;i<1000;i++){
		nnet_cc_quickprop(&bestcan,&best);
		nnet_cc_train(&bestcan,&best);
		if(!(i%100)) printf("Quickprop iter %d correlation %f\n",
			i,bestcan.fitness);
 		if(stagsse<bestcan.fitness){
		   stagsse=bestcan.fitness;
	           stag = 0;
		   copy_wt(&bestcan2,&bestcan);	
		}
		else stag++;
		if(stag>20) break;
	}

 	copy_wt(&bestcan,&bestcan2);	

	printf("Best correlation after %d Quickprop iter: %f\n",
		i-stag,bestcan.fitness);

/*        printf("bestcan\n");
	nnet_disp(&bestcan);
*/
  /*    printf("before adding bestcan:\n");	

    	printf("pop1\n");
    	pop_disp(pop1);
    	printf("pop2\n");
    	pop_disp(pop2);	
    	printf("pop3\n");
    	pop_disp(pop3);
    	printf("pop4\n");
    	pop_disp(pop4);
  */

	/* INSTALL BEST CANDIDATE */

	printf("\007Adding hidden node...\n");

	++numhidden;	

	nnet_link_to_candidate(&minwt,&bestcan);
	nnet_link_to_candidate(&maxwt,&bestcan);

	min_weight = -10.0;	
	max_weight = 10.0;	

    	for(neur=minwt.outputlist;neur!=NULL;neur=neur->nextneur) 
    		for(syn=neur->synlist;syn!=NULL;syn=syn->nextsyn) 
    			syn->weight=min_weight;

    	for(neur=maxwt.outputlist;neur!=NULL;neur=neur->nextneur) 
    		for(syn=neur->synlist;syn!=NULL;syn=syn->nextsyn) 
    			syn->weight=max_weight;

/*	printf("min\n");	
	nnet_disp(&minwt);	
*/
/*	printf("max\n");	
	nnet_disp(&maxwt);	
*/

	for(i=0;i<pop_size;i++){
            nnet_link_to_candidate(&pop1[i],&bestcan);
            nnet_link_to_candidate(&pop2[i],&bestcan);
   	    nnet_link_to_candidate(&pop3[i],&bestcan); 
   	    nnet_link_to_candidate(&pop4[i],&bestcan); 
        }	

	nnet_link_to_candidate(&bestcan2,&bestcan);
	nnet_link_to_candidate(&best2,&bestcan);

/*      printf("pop1\n");
	pop_disp(pop1);
        printf("pop2\n");
	pop_disp(pop2);
 	printf("pop3\n");
	pop_disp(pop3);
	printf("pop4\n");
	pop_disp(pop4);
*/  

        nnet_add_cc_neuron(&best,&bestcan);

/*        printf("neuron added to best\n"); */
/*        nnet_disp(&best);
*/
        nnet_cc_candidate(&bestcan,&best);	
	
    }
	
} 
