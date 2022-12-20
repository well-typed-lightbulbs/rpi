/*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
   --------------------------------------------------------------------------*/

#include <stdint.h>
#include<stdio.h>

/* Memory barriers */

static inline void barrier_dsb (void) { __asm ("dsb sy"); }
static inline void barrier_dmb (void) { __asm ("dmb sy"); }
static inline void barrier_isb (void) { __asm ("isb"); }
static inline void barrier_wait (uint32_t d)
{
  for (volatile uint32_t i = 0; i < d; i++) ;
}

/* Memory reads */

static inline uint8_t mem_get_u8 (uint64_t r)
{
  return *(volatile uint8_t *)r;
}

static inline uint32_t mem_get_u32 (uint64_t r)
{
  return *(volatile uint32_t *)r;
}

static inline uint64_t mem_get_u64 (uint64_t r)
{
  return *(volatile uint64_t *)r;
}

/* Memory writes */

static inline void mem_set_u8 (uint64_t r, uint8_t v)
{
  *(volatile uint8_t *)r = v;
}

static inline void mem_set_u32 (uint64_t r, uint32_t v)
{
  *(volatile uint32_t *)r = v;
}

static inline void mem_set_u64 (uint64_t r, uint64_t v)
{
  *(volatile uint64_t *)r = v;
}

/* Memory masked writes */

static inline void mem_set_u8_bits (uint64_t r, uint8_t bits, uint8_t v)
{
  uint8_t current = mem_get_u8 (r);
  v = (current & ~bits) | (v & bits);
  mem_set_u8 (r, v);
}

static inline void mem_set_u32_bits (uint64_t r, uint32_t bits, uint32_t v)
{
  uint32_t current = mem_get_u32 (r);
  v = (current & ~bits) | (v & bits);
  mem_set_u32 (r, v);
}

static inline void mem_set_u64_bits (uint64_t r, uint64_t bits, uint64_t v)
{
  uint64_t current = mem_get_u64 (r);
  v = (current & ~bits) | (v & bits);
  mem_set_u64 (r, v);
}



/* OCaml stubs */

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/bigarray.h>


/* Memory barriers */

value ocamlrpi_barrier_dsb (value unit) { barrier_dsb (); return Val_unit; }
value ocamlrpi_barrier_dmb (value unit) { barrier_dmb (); return Val_unit; }
value ocamlrpi_barrier_isb (value unit) { barrier_isb (); return Val_unit; }
value ocamlrpi_barrier_wait (value d)
{
  barrier_wait (Int_val (d));
  return Val_unit;
}

#ifdef __aarch64__
  #define Optint_val(addr) (Long_val(addr))
#else
  #error "32bit?"
#endif


/* Reads */

value ocamlrpi_mem_get_byte (value addr)
{
  return Val_int (mem_get_u8 (Optint_val (addr)));
}

value ocamlrpi_mem_get_int (value addr)
{
  return Val_int (mem_get_u32 (Optint_val (addr)));
}

value ocamlrpi_mem_get_int32 (value addr)
{
  return caml_copy_int32 (mem_get_u32 (Optint_val (addr)));
}

value ocamlrpi_mem_get_int64 (value addr)
{
  return caml_copy_int64 (mem_get_u64 (Optint_val (addr)));
}

/* Writes */

value ocamlrpi_mem_set_byte (value addr, value v)
{
  mem_set_u8 (Optint_val (addr), Int_val (v));
  return Val_unit;
}

value ocamlrpi_mem_set_int (value addr, value v)
{
  mem_set_u32 (Optint_val (addr), Int_val (v));
  return Val_unit;
}

value ocamlrpi_mem_set_int32 (value addr, value v)
{
  mem_set_u32 (Optint_val (addr), Int32_val (v));
  return Val_unit;
}

value ocamlrpi_mem_set_int64 (value addr, value v)
{
  mem_set_u64 (Optint_val (addr), Int64_val (v));
  return Val_unit;
}

value ocamlrpi_mem_set_int32_pow (value addr, value pow)
{
  mem_set_u32 (Optint_val (addr), 1 << Int_val (pow));
  return Val_unit;
}

/* Masked writes */

value ocamlrpi_mem_set_byte_bits (value addr, value bits, value v)
{
  mem_set_u8_bits (Optint_val (addr), Int_val (bits), Int_val (v));
  return Val_unit;
}

value ocamlrpi_mem_set_int_bits (value addr, value bits, value v)
{
  mem_set_u32_bits (Optint_val (addr), Int_val (bits), Int_val (v));
  return Val_unit;
}

value ocamlrpi_mem_set_int32_bits (value addr, value bits, value v)
{
  mem_set_u32_bits (Optint_val (addr), Int32_val (bits), Int32_val (v));
  return Val_unit;
}

value ocamlrpi_mem_set_int64_bits (value addr, value bits, value v)
{
  mem_set_u32_bits (Optint_val (addr), Int64_val (bits), Int64_val (v));
  return Val_unit;
}

/*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli.
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
   --------------------------------------------------------------------------*/
