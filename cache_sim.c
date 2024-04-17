#include <stdlib.h>
#include <stdio.h>
#include <omp.h>
#include <string.h>
#include <ctype.h>

typedef char byte;

struct cache {
    byte address;   // This is the address in memory.
    byte value;    // This is the value stored in cached memory.
    byte state;    // State for MESI protocol (M, E, S, I)
};

struct decoded_inst {
    int type;      // 0 is RD, 1 is WR
    byte address;
    byte value;    // Only used for WR
};

typedef struct cache cache;
typedef struct decoded_inst decoded;

byte *memory; // Global memory area

// Decode instruction lines
decoded decode_inst_line(char *buffer) {
    decoded inst;
    char inst_type[2];
    sscanf(buffer, "%s", inst_type);
    if (!strcmp(inst_type, "RD")) {
        int addr = 0;
        sscanf(buffer, "%s %d", inst_type, &addr);
        inst.type = 0;
        inst.value = -1;
        inst.address = addr;
    } else if (!strcmp(inst_type, "WR")) {
        int addr = 0;
        int val = 0;
        sscanf(buffer, "%s %d %d", inst_type, &addr, &val);
        inst.type = 1;
        inst.address = addr;
        inst.value = val;
    }
    return inst;
}

// Helper function to print the cachelines (for debugging)
void print_cachelines(cache *c, int cache_size) {
    for (int i = 0; i < cache_size; i++) {
        cache cacheline = *(c + i);
        printf("Address: %d, State: %c, Value: %d\n", cacheline.address, cacheline.state, cacheline.value);
    }
}

// This function implements the mock CPU loop that reads and writes data
void cpu_loop(int num_threads) {
    // Initialize a CPU level cache (2 bytes for simplicity)
    int cache_size = 2;
    cache *c = (cache *) malloc(sizeof(cache) * cache_size * num_threads);

    // Initialize cache lines to invalid state
    for (int i = 0; i < cache_size * num_threads; i++) {
        (c + i)->state = 'I';
        (c + i)->address = -1;
    }

    #pragma omp parallel
    {
        // Read Input file (specific to each thread)
        int thread_num = omp_get_thread_num();

        char filename[20];
        sprintf(filename, "input_%d.txt", thread_num);
        FILE *inst_file = fopen(filename, "r");

        char inst_line[20];

        // Decode instructions and execute them
        while (fgets(inst_line, sizeof(inst_line), inst_file)) {
            decoded inst = decode_inst_line(inst_line);

            /*
            * Cache Replacement Algorithm (Direct Mapped)
            */
            int hash = inst.address % cache_size;
            cache *cacheline = c + thread_num * cache_size + hash;

            //Handle cache miss
            if( cacheline->address != inst.address){
                if( cacheline->state == 'M'){
                    #pragma omp critical
                    {
                        memory[cacheline->address] = cacheline->value; // Write back to memory if modified
                    }
                }
                cacheline->address = inst.address; // Update address
                cacheline->state = 'I'; // Invalidate cache line
                cacheline->value = -1; // Reset value
            }

            /*
            * MESI Protocol Implementation
            */
             switch (inst.type) {
                case 0: // RD (Read)
                    switch (cacheline->state) {
                        case 'M': // Modified
                            // Read hit, no bus transaction needed
                            break;
                        case 'E': // Exclusive
                            // Read hit, no bus transaction needed
                            break;
                        case 'S': // Shared
                            // Read hit, no bus transaction needed
                            break;
                        case 'I': // Invalid
                            #pragma omp critical
                            {
                                int found = 0;
                                for (int i = 0; i < num_threads; i++) {
                                    if (i != thread_num) {
                                        cache *other_cacheline = c + i * cache_size + hash;
                                        if (other_cacheline->state != 'I' && other_cacheline->address == inst.address) {
                                            // Data found in another cache, transition to Shared state
                                            if(!found){
                                                cacheline->value = other_cacheline->value;
                                                cacheline->address = inst.address;
                                                cacheline->state = 'S';
                                                memory[inst.address] = other_cacheline->value;
                                            }
                                            if( other_cacheline->state == 'M' || other_cacheline->state == 'E'){
                                                other_cacheline->state = 'S'; // Transition other cache to Shared state
                                            }
                                            found = 1;
                                        }
                                    }
                                }

                                if (!found) {
                                    // Address not found in other caches, fetch from memory and transition to Exclusive state
                                    cacheline->value = memory[inst.address];
                                    cacheline->address = inst.address;
                                    cacheline->state = 'E';
                                }
                            }
                            break;
                    }

                    #pragma omp critical
                    {   
                        //print_cachelines(c, cache_size * num_threads);
                        printf("Thread %d: RD %d: %d\n", thread_num, inst.address, cacheline->value);
                    }
                    break;

                case 1: // WR (Write)
                    switch (cacheline->state) {
                        case 'M': // Modified
                            // Write hit, update value and keep state as Modified
                            break; 
                        case 'E': // Exclusive
                            // Write hit, update value and transition to Modified
                            cacheline->state = 'M'; 
                            break;
                        case 'S': // Shared 
                            // Invalidate other copies and transition to Modified
                            #pragma omp critical 
                            {
                                for (int i = 0; i < num_threads; i++) {
                                    if (i != thread_num) {
                                        cache *other_cacheline = c + i * cache_size + hash;
                                        if (other_cacheline->address == inst.address) {
                                            other_cacheline->state = 'I'; // Invalidate other copies
                                        }
                                    }
                                }
                                cacheline->state = 'M'; // Transition to Modified
                            }
                            break;
                        case 'I': // Invalid
                            // Fetch data and invalidate other copies, then transition to Modified
                            #pragma omp critical
                            {   
                                int found=0;
                                for (int i = 0; i < num_threads; i++) {
                                    if (i != thread_num) {
                                        cache *other_cacheline = c + i * cache_size + hash;
                                        if (other_cacheline->state != 'I' && other_cacheline->address == inst.address) {
                                            cacheline->value = other_cacheline->value;
                                            cacheline->address = inst.address;
                                            other_cacheline->state = 'I'; // Invalidate other copies
                                            if(!found){
                                                memory[inst.address] = other_cacheline->value;
                                            }
                                            found = 1;
                                        }
                                    }
                                }
                                if( !found){
                                    cacheline->value = memory[inst.address]; // Fetch from memory
                                    cacheline->address = inst.address;                                 
                                }
                                cacheline->state = 'M'; // Transition to Modified 
                            }
                            break;
                    } 
                    cacheline->value = inst.value;
                    cacheline->address = inst.address; 
                    #pragma omp critical
                    {
                       // print_cachelines(c, cache_size * num_threads);
                        printf("Thread %d: WR %d: %d\n", thread_num, inst.address, cacheline->value); 
                    }
                    break;
            }
            // Update cache line
            *(c + hash) = *cacheline;
        }
    }
    free(c);
}

int main(int argc, char *argv[]) {
    // Initialize Global memory (24 bytes for simplicity)
    int memory_size = 24;
    memory = (byte *) malloc(sizeof(byte) * memory_size);
    // Initialize memory values to -1
    for (int i = 0; i < memory_size; i++) {
        *(memory + i) = -1;
    }

    // Get number of threads from command line argument (default is 2)
    int num_threads = 2;
    if (argc > 1) {
        num_threads = atoi(argv[1]);
    }

    cpu_loop(num_threads);

    free(memory);
    return 0;
}