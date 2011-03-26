#include <stdio.h>
#include <string.h>
#include <math.h>

#include "erl_nif.h"
#include "erl_nif_compat.h"
#include "yajl/yajl_encode.h"



typedef struct {
    ErlNifEnv* env;
    ErlNifBinary bin;
    size_t fill_offset;
    int fatal_error;
} encode_ctx;


static int
ensure_buffer(void* vctx, unsigned int len) {
    encode_ctx* ctx = (encode_ctx*)vctx;
    if ((ctx->bin.size - ctx->fill_offset) < len) {
        if(!enif_realloc_binary(&(ctx->bin), (ctx->bin.size * 2) + len)) {
            return 0;
        }
    }
    return 1;
}

static void
fill_buffer(void* vctx, const char* str, unsigned int len)
{
    encode_ctx* ctx = (encode_ctx*)vctx;
    if (ctx->fatal_error) return;
    
    if (!ensure_buffer(vctx, len)) {
        ctx->fatal_error = 1;
    }
    memcpy(ctx->bin.data + ctx->fill_offset, str, len);
    ctx->fill_offset += len;
}

/* Json encode the string binary into the ctx.bin,
  with surrounding quotes and all */
static int
encode_string(void* vctx, ERL_NIF_TERM binary)
{
    encode_ctx* ctx = (encode_ctx*)vctx;
    ErlNifBinary bin;
    
    if(!enif_inspect_binary(ctx->env, binary, &bin)) {
        return 0;
    }
    fill_buffer(ctx, "\"", 1);
    if (ctx->fatal_error) {
        return 0;
    }
    yajl_string_encode2(fill_buffer, ctx, bin.data, bin.size);
    fill_buffer(ctx, "\"", 1);
    
    if (ctx->fatal_error) {
        return 0;
    }
    return 1;
}

static ERL_NIF_TERM
no_mem_error(ErlNifEnv* env)
{
    return enif_make_tuple(env, 2,
            enif_make_atom(env, "error"),
            enif_make_atom(env, "insufficient_memory"));
}

ERL_NIF_TERM
final_encode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM head = argv[0];
    ERL_NIF_TERM term;

    double number;
    
    encode_ctx ctx;
    ctx.env = env;
    ctx.fill_offset = 0;
    ctx.fatal_error = 0;
    
    if (!enif_alloc_binary(100, &ctx.bin)) { 
            return no_mem_error(env);
    }
    
    while(enif_get_list_cell(env, head, &term, &head)) {
        ErlNifBinary termbin;
        
        // We scan the list, looking for things to write into the binary, or
        // encode and then write into the binary. We encode values that are
        // tuples tagged with a type and a value: {Type, Value} where Type
        // is a an Integer and Value is what is to be encoded
        
        if (enif_is_tuple(env, term)) {
            // It's a tuple.
            const ERL_NIF_TERM* array;
            int arity;
            int code;
            if (!enif_get_tuple(env, term, &arity, &array)) {
                return enif_make_badarg(env);
            }
            if (arity != 2 || !enif_get_int(env, array[0], &code)) {
                return enif_make_badarg(env);
            }
            if (code == 0) {
                // {0, String}
                if (!encode_string(&ctx, array[1])) {
                    return no_mem_error(env);
                }
            }
            else {
                // {1, Double}
                if(!enif_get_double(env, array[1], &number)) {
                    return enif_make_badarg(env);
                }
                if (isnan(number) || isinf(number)) {
                    return enif_make_badarg(env);
                }
                if (!ensure_buffer(&ctx, 32)) {
                    return no_mem_error(env);
                }
                // write the string into the buffer
                snprintf((char*)ctx.bin.data+ctx.fill_offset, 32,
                        "%.16g", number);
                // increment the length
                ctx.fill_offset += strlen((char*)ctx.bin.data+ctx.fill_offset);
            }
            
        } else if (enif_inspect_binary(env, term, &termbin)) {    
            fill_buffer(&ctx, (char*)termbin.data, termbin.size);
            if (ctx.fatal_error) {
                return no_mem_error(env);
            }
        }
        else {
            //not a binary, not a tuple, wtf!
            return enif_make_badarg(env);
        }
    }
    if(!enif_realloc_binary(&(ctx.bin), ctx.fill_offset)) {
        return no_mem_error(env);
    }
    
    return enif_make_binary(env, &ctx.bin);
}

