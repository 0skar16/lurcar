# ==Transferred to [Codeberg](https://codeberg.org/moonydev/lurcar)==

# LURCAR

The origin of the name: LU (R)andom a(C)cess (AR)chive

It was supposed to be a straight forward archive for personal projects because nothing really fit my needs. It was built on top of the corpse of [QRCA](https://github.com/0skar16/qrca), which I brought back and archived for reference. Yet through the long development to adapt it to my needs, it became more of a file system. Whatever the case, if this is something you need, here you go.

# Features:
- Unallocated space that can be dinamically allocated for a new file
- Padding to not need to rebuild the whole archive/leaving lots of unallocated space
- Optional checksums
- Extra fields and a hashset for each file and the whole archive
- One instance for writing and reading an entry, no more Reader and Writer structs for not needed complexity. With a selection of different options based on the traits of the inner head, it should work with an inner that has Read and/or Write (it can use Seek too!)
- The directory is located in unallocated space instead of the beginning or end of the archive. If there's not enough unallocated space, it will still be placed at the end, but marked as unallocated space so a file still can use it without getting confused and doing the thing that it did in older versions of LURCAR
- The directory can be included in the file or supplied externally
- And of course don't forget about the utter lack of documentation (as it happens to be very often with my personal projects)
- Probably something else, but I can't remember
