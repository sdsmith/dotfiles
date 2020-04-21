/*
 * \file Fast way to find if the given directory is part of a mercurial
 * repository.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/errno.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#define likely(x)      __builtin_expect(!!(x), 1)
#define unlikely(x)    __builtin_expect(!!(x), 0)

#define MAX_PATH_LEN 1024

int main(int argc, char *argv[]) {
    if (unlikely(argc != 2)) {
        printf("Usage: in_hg <path>\n");
        return 1;
    }

    const char *loc = argv[1];
    const char hg_dir_postfix[] = "/.hg";
    char path[MAX_PATH_LEN];
    if (unlikely(chdir(loc) < 0)) {
        perror("chdir");
        exit(1);
    }
    if (unlikely(!getcwd(path, sizeof(path) - sizeof(hg_dir_postfix)))) {
        perror("getcwd");
        exit(1);
    }

    char *null_pos = path + strlen(path);
    struct stat s;
    int err = 0;
    while (likely(path[0] != '\0')) {
        // Copy the postfix in
        memcpy(null_pos, hg_dir_postfix, sizeof(hg_dir_postfix));
        *(null_pos + sizeof(hg_dir_postfix)) = '\0';

        // Check if exists
        if (likely(stat(path, &s) < 0)) {
            if (errno != ENOENT) {
                perror("stat");
                exit(1);
            }
        } else if (unlikely(S_ISDIR(s.st_mode))) {
            exit(0); // Found it!
        }

        // Remove the last directory entry
        *null_pos = '\0';
        char *last_slash = strrchr(path, '/');
        if (unlikely(!last_slash)) {
            // At root dir, skip checking it
            break;
        }
        null_pos = last_slash;
        *null_pos = '\0';
    }

    exit(1);
}
