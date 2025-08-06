#include "cache.h"
#include "dogfault.h"
#include <assert.h>
#include <ctype.h>
#include <getopt.h>
#include <math.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

// DO NOT MODIFY THIS FILE. INVOKE AFTER EACH ACCESS FROM runTrace
void print_result(result r) {
  if (r.status == CACHE_EVICT)
    printf(" [status: miss eviction, victim_block: 0x%llx, insert_block: 0x%llx]",
           r.victim_block_addr, r.insert_block_addr);
  if (r.status == CACHE_HIT)
    printf(" [status: hit]");
  if (r.status == CACHE_MISS)
    printf(" [status: miss, insert_block: 0x%llx]", r.insert_block_addr);
}

/* This is the entry point to operate the cache for a given address in the trace file.
 * First, is increments the global lru_clock in the corresponding cache set for the address.
 * Second, it checks if the address is already in the cache using the "probe_cache" function.
 * If yes, it is a cache hit:
 *     1) call the "hit_cacheline" function to update the counters inside the hit cache 
 *        line, including its lru_clock and access_counter.
 *     2) record a hit status in the return "result" struct and update hit_count 
 * Otherwise, it is a cache miss:
 *     1) call the "insert_cacheline" function, trying to find an empty cache line in the
 *        cache set and insert the address into the empty line. 
 *     2) if the "insert_cacheline" function returns true, record a miss status and the
          inserted block address in the return "result" struct and update miss_count
 *     3) otherwise, if the "insert_cacheline" function returns false:
 *          a) call the "victim_cacheline" function to figure which victim cache line to 
 *             replace based on the cache replacement policy (LRU and LFU).
 *          b) call the "replace_cacheline" function to replace the victim cache line with
 *             the new cache line to insert.
 *          c) record an eviction status, the victim block address, and the inserted block
 *             address in the return "result" struct. Update miss_count and eviction_count.
 */
 
result operateCache(const unsigned long long address, Cache *cache) {
  result r;
  r.status = CACHE_MISS;
  r.victim_block_addr = 0;
  r.insert_block_addr = 0;
  
  // get the set index for this address
  unsigned long long set_index = cache_set(address, cache);
  
  //increment the LRU clock for the set
  cache->sets[set_index].lru_clock++;
  
  //see if the address is in the cache
  if (probe_cache(address, cache)) {
    // cache hit
    hit_cacheline(address, cache);
    r.status = CACHE_HIT;
    cache->hit_count++;
  } else {
    // cache miss - try to insert into an empty line
    if (insert_cacheline(address, cache)) {
      // successfully inserted into empty line
      r.status = CACHE_MISS;
      r.insert_block_addr = address_to_block(address, cache);
      cache->miss_count++;
    } else {
      // no empty line - need to evict
      unsigned long long victim_addr = victim_cacheline(address, cache);
      replace_cacheline(victim_addr, address, cache);
      r.status = CACHE_EVICT;
      r.victim_block_addr = victim_addr;
      r.insert_block_addr = address_to_block(address, cache);
      cache->miss_count++;
      cache->eviction_count++;
    }
  }
  
  return r;
}
// HELPER FUNCTIONS USEFUL FOR IMPLEMENTING THE CACHE
// Given an address, return the block (aligned) address,
// i.e., byte offset bits are cleared to 0
//[    TAG BITS    ][SET INDEX BITS][BLOCK OFFSET BITS]
//memory to block address getting rid of offset bits

unsigned long long address_to_block(const unsigned long long address,
                                const Cache *cache) {
  unsigned long long block_mask = ~((1ULL << cache->blockBits) - 1);
  return address & block_mask; // returns the and to only give the important bits using the mask which are the bottom bits
}

//simply extracts the tag bits from the memory address from the front
unsigned long long cache_tag(const unsigned long long address,
                             const Cache *cache) {
  //removes the set and block bits by shifting the address
  int tag_shift = cache->setBits + cache->blockBits;
  return address >> tag_shift;
}

//simply extracts the set index bits from the middle
unsigned long long cache_set(const unsigned long long address,
                             const Cache *cache) {
  // extract set index bits (middle bits)
  unsigned long long set_mask = (1ULL << cache->setBits) - 1;
  //shifts it to remove block bits than uses a mask to remove the tag bits
  return (address >> cache->blockBits) & set_mask;
}

// check if the address is found in the cache
bool probe_cache(const unsigned long long address, const Cache *cache) {
  unsigned long long set_index = cache_set(address, cache);
  unsigned long long tag = cache_tag(address, cache);
  
  Set *set = &cache->sets[set_index];
  
  // check all lines in the set
  for (int i = 0; i < cache->linesPerSet; i++) {
    if (set->lines[i].valid && set->lines[i].tag == tag) {
      return true;
    }
  }
  
  return false;
}

// Access address in cache called only if probe is successful updates LRU and LFU accordingly
void hit_cacheline(const unsigned long long address, Cache *cache) {
  unsigned long long set_index = cache_set(address, cache);
  unsigned long long tag = cache_tag(address, cache);
  
  Set *set = &cache->sets[set_index];
  for (int i = 0; i < cache->linesPerSet; i++) {
    if (set->lines[i].valid && set->lines[i].tag == tag) {
      // LRU clock making it recently used value
      set->lines[i].lru_clock = set->lru_clock;
      // update access counter for LFU checking for frequency of use
      set->lines[i].access_counter++;
      return;
    }
  }
}

/* This function is only called if probe_cache returns false, i.e., the address is
 * not in the cache. In this function, it will try to find an empty (i.e., invalid)
 * cache line for the address to insert. 
 * If it found an empty one:
 *     1) it inserts the address into that cache line (marking it valid).
 *     2) it updates the cache line's lru_clock based on the global lru_clock 
 *        in the cache set and initiates the cache line's access_counter.
 *     3) it returns true.
 * Otherwise, it returns false.  
 */ 
bool insert_cacheline(const unsigned long long address, Cache *cache) {
  unsigned long long set_index = cache_set(address, cache);
  unsigned long long tag = cache_tag(address, cache);
  unsigned long long block_addr = address_to_block(address, cache);
  
  Set *set = &cache->sets[set_index];
  
  // look for an empty line
  for (int i = 0; i < cache->linesPerSet; i++) {
    if (!set->lines[i].valid) {
      // found empty line - intialize the values
      set->lines[i].valid = true;
      set->lines[i].tag = tag;
      set->lines[i].block_addr = block_addr;
      set->lines[i].lru_clock = set->lru_clock;
      set->lines[i].access_counter = 1;
      return true;
    }
  }
  
  return false; // no empty line found
}

//checks what cache lines need to be removed when it is full
//LRU least recnetly used
//LFU least frequently used
//returns the block address of the rmeoved cacheline
unsigned long long victim_cacheline(const unsigned long long address,
                                const Cache *cache) {
  unsigned long long set_index = cache_set(address, cache);
  Set *set = &cache->sets[set_index];
  
  int victim_index = 0;
  
  if (cache->lfu == 0) {
    // LRU policy - find line with smallest lru_clock mens least recently used
    unsigned long long min_lru = set->lines[0].lru_clock;
    //loops to find the lowest than sets the set_index of the to be removed 
    for (int i = 1; i < cache->linesPerSet; i++) {
      if (set->lines[i].lru_clock < min_lru) {
        min_lru = set->lines[i].lru_clock;
        victim_index = i;
      }
    }
  } else {
    //LFU policy - find line with smallest access_counter least frequently used
    // use LRU if both are the same
    int min_access = set->lines[0].access_counter;
    unsigned long long min_lru = set->lines[0].lru_clock;
    
    for (int i = 1; i < cache->linesPerSet; i++) {
      if (set->lines[i].access_counter < min_access ||
          (set->lines[i].access_counter == min_access && 
           set->lines[i].lru_clock < min_lru)) {
        min_access = set->lines[i].access_counter;
        min_lru = set->lines[i].lru_clock;
        victim_index = i;
      }
    }
  }
  
  return set->lines[victim_index].block_addr;
}

 //replace the to be rmoeved cache line with a new address to be inserted
void replace_cacheline(const unsigned long long victim_block_addr,
               const unsigned long long insert_addr, Cache *cache) {
  unsigned long long set_index = cache_set(insert_addr, cache);
  unsigned long long tag = cache_tag(insert_addr, cache);
  unsigned long long block_addr = address_to_block(insert_addr, cache);
  
  Set *set = &cache->sets[set_index];
  
  // Find the victim line by its block address
  for (int i = 0; i < cache->linesPerSet; i++) {
    if (set->lines[i].block_addr == victim_block_addr) {
      // Replace this line
      set->lines[i].valid = true;
      set->lines[i].tag = tag;
      set->lines[i].block_addr = block_addr;
      set->lines[i].lru_clock = set->lru_clock;
      set->lines[i].access_counter = 1;
      return;
    }
  }
}

// allocate the memory space for the cache with the given cache parameters
// initializes all cache values

void cacheSetUp(Cache *cache, char *name) {
  // calculate number of sets
  int num_sets = 1 << cache->setBits; // 2^setBits
  
  // allocate memory for sets
  cache->sets = (Set*)malloc(num_sets * sizeof(Set));
  
  // initialize each set
  for (int i = 0; i < num_sets; i++) {
    cache->sets[i].lru_clock = 0;
    cache->sets[i].lines = (Line*)malloc(cache->linesPerSet * sizeof(Line));
    
    // initialize each line in the set
    for (int j = 0; j < cache->linesPerSet; j++) {
      cache->sets[i].lines[j].valid = false;
      cache->sets[i].lines[j].tag = 0;
      cache->sets[i].lines[j].block_addr = 0;
      cache->sets[i].lines[j].lru_clock = 0;
      cache->sets[i].lines[j].access_counter = 0;
    }
  }
  
  // set cache name
  cache->name = (char*)malloc(strlen(name) + 1);
  strcpy(cache->name, name);
}

// deallocate the memory space for the cache
void deallocate(Cache *cache) {
  int num_sets = 1 << cache->setBits;
  
  // free lines for each set
  for (int i = 0; i < num_sets; i++) {
    free(cache->sets[i].lines);
  }
  
  // free sets array
  free(cache->sets);
  
  // free cache name
  free(cache->name);
}

// print out summary stats for the cache
void printSummary(const Cache *cache) {
  printf("%s hits: %d, misses: %d, evictions: %d\n", cache->name, cache->hit_count,
         cache->miss_count, cache->eviction_count);
}
