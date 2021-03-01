#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#define allocate malloc
#define deallocate free

typedef struct {
  void** data;
  uint64_t capacity, length;
} List;

List list_make(void) {
  List self;
  self.data = NULL;
  self.capacity = 0;
  self.length = 0;
  return self;
}

List* list_new(void) {
  List* self = (List*)allocate(sizeof(List));

  if (!self) {
    fprintf(stderr, "error (%s)", "Failed to allocate memory for list");
    exit(1);
  }

  self->data = NULL;
  self->capacity = 0;
  self->length = 0;
  return self;
}

void list_push(List* self, void* item) {
  if (self->data == NULL) {
    self->data = malloc(sizeof(void*));
    self->capacity += 1;
    self->data[self->length++] = item;
    return;
  }

  if (self->length == self->capacity) {
    self->capacity *= 2;
    self->data = realloc(self->data, sizeof(void*) * self->capacity);
    memset(self->data + self->length, 0, (size_t)(self->capacity - 1) * sizeof(void*));
  }

  self->data[self->length++] = item;
}

int main() {
  List strs = list_make();

  for (uint64_t i = 0; i < 1000; i++) {
    list_push(&strs, (void*)i);
  }

  void** it = strs.data;

  int i = 0;
  while (it != NULL) {
    const uint64_t d = (uint64_t)(*it);
    /* printf("%d %lu\n", i++, d); */
    it++;
  }

  return 0;
}
