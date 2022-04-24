#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <stdio.h>
#include <sys/mman.h>

#define MEMSZ ((size_t) 0x10000)
static uint16_t mem[MEMSZ + 1] = {0};
static uint16_t *external_memory = NULL;

static void dump_external(
    uint32_t a,
    uint32_t b,
    uint16_t n,
    uint16_t addr
) {
    if (n == 0) return;
    bool mem_wraparound = (uint16_t)(addr + n) < addr;
    if (!external_memory) {
        if (!mem_wraparound)
            memset(mem + addr, 0, n * sizeof(uint16_t));
        else
            for (; n; n--) mem[addr++] = 0;
        return;
    }
    uint32_t external_addr = a | (b << 16);
    bool ext_wraparound = (uint32_t)(external_addr + n) < external_addr;
    if (!mem_wraparound && !ext_wraparound)
        memcpy(mem + addr, external_memory + external_addr, n * sizeof(uint16_t));
    else
        for (; n; n--) mem[addr++] = external_memory[external_addr++];
}

static void save_external(
    uint32_t a,
    uint32_t b,
    uint16_t n,
    uint16_t addr
) {
    if (n == 0) return;
    if (!external_memory) {
        external_memory = mmap(
            NULL,
            MEMSZ * MEMSZ * sizeof(uint16_t), // size to allocate (8gb)
            PROT_READ | PROT_WRITE, // protections
            MAP_PRIVATE | MAP_ANONYMOUS, // new memory
            -1, // no file descriptor since this isn't a file
            0 // no offset since this isn't a file
        );
        if (external_memory == MAP_FAILED) {
            perror("error(mmap)");
            exit(EXIT_FAILURE);
        }
    }
    uint32_t external_addr = a | (b << 16);
    bool mem_wraparound = (uint16_t)(addr + n) < addr;
    bool ext_wraparound = (uint32_t)(external_addr + n) < external_addr;
    if (!mem_wraparound && !ext_wraparound)
        memcpy(external_memory + external_addr, mem + addr, n * sizeof(uint16_t));
    else
        for (; n; n--) external_memory[external_addr++] = mem[addr++];
}

int main(int argc, char **argv) {
    {
        char *file_name;
        bool quiet = false;
        if (argc == 2) {
            file_name = argv[1];
        } else if (argc == 3 && !strcmp(argv[1], "-q")) {
            quiet = true;
            file_name = argv[2];
        } else {
            char *program_name = argv[0];
            fprintf(stderr,
                "Usage:\n"
                "  %s [file]     load and run image file\n"
                "  %s -q [file]  suppress warnings\n",
                program_name, program_name);
            return EXIT_FAILURE;
        }
        FILE *f = fopen(file_name, "r");
        if (!f) {
            perror("error");
            return EXIT_FAILURE;
        }
        size_t bytes_read = fread(mem, 1, sizeof(uint16_t) * MEMSZ, f);
        if (errno) {
            fclose(f);
            perror("error");
            return EXIT_FAILURE;
        }
        if (!quiet) {
            char const *warning = NULL;
            if (bytes_read < 0x20000)
                warning = "warning: The image file was smaller than 0x20000 bytes. "
                          "The remaining memory will be initialized to zero.\n";
            if (bytes_read > 0x20000)
                warning = "warning: The image file was too large."
                          "All bytes after position 0x20000 have been ignored.\n";
            if (warning)
                fputs(warning, stderr);
        }
        fclose(f);
    }

    // 'ip' contains the up-to-date value of what
    // the first word of memory ought to be
    uint16_t ip = mem[0];
    for (;;) {
        uint16_t opcode = mem[ip];
#define ARG(n) (mem[(uint16_t)(ip + n)])
        switch (opcode) {
            case 0: // hlt
                return EXIT_SUCCESS;
            case 2: // get
                mem[0] = ip + 2;
                ARG(1) = mem[ARG(1)];
                ip = mem[0];
                break;
            case 3: // lit
                mem[0] = ip + 3;
                mem[ARG(2)] = ARG(1);
                ip = mem[0];
                break;
            case 4: // not
                // works without syncing by coincidence
                if (ip == MEMSZ - 1) return EXIT_SUCCESS;
                ARG(1) = !ARG(1);
                ip += 2;
                break;
            case 5: // add
                mem[0] = ip + 3;
                ARG(1) += ARG(2);
                ip = mem[0];
                break;
            case 6: // mul
                mem[0] = ip + 3;
                ARG(1) *= ARG(2);
                ip = mem[0];
                break;
            case 7: // max
                mem[0] = ip + 3;
                {
                    uint16_t a = ARG(1), b = ARG(2);
                    if (b > a) ARG(1) = b;
                }
                ip = mem[0];
                break;
            case 8: // dmp
                mem[0] = ip + 4;
                dump_external(ARG(1), ARG(2), ARG(3), ip + 4);
                ip = mem[0];
                break;
            case 9: // sav
                mem[0] = ip + 4;
                save_external(ARG(1), ARG(2), ARG(3), ip + 4);
                ip = mem[0];
                break;
            case 10: // chr
                putchar(ip == MEMSZ - 1 ? 1 : (uint8_t) ARG(1));
                ip += 2;
                break;
            case 11: // chr
                printf("%d", ip == MEMSZ - 1 ? 1 : ARG(1));
                ip += 2;
                break;
            case 12: // chi
                {
                    int input = getchar();
                    if (input == EOF) input = -1;
                    if (ip == MEMSZ - 1) ip = input;
                    else { ARG(1) = input; ip += 2; }
                }
                break;
            case 1: // nop
            default:
                ip++;
                break;
        }
    }
}
