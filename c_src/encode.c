#include <stdio.h>
#include <string.h>

#include "erl_nif.h"
#include "erl_nif_compat.h"
#include "yajl/yajl_encode.h"



typedef struct {
    ErlNifEnv* env;
    ErlNifBinary bin;
    size_t fill_offset;
    int fatal_error;
} encode_ctx;


static void
fill_buffer(void* vctx,
                             const char* str,
                             unsigned int len)
{
    encode_ctx* ctx = (encode_ctx*)vctx;
    if (ctx->fatal_error) return;
    if ((ctx->bin.size - ctx->fill_offset) < len) {
        if(!enif_realloc_binary(&(ctx->bin), (ctx->bin.size * 2) + len)) {
            ctx->fatal_error = 1;
            return;
        }
    }
    memcpy(ctx->bin.data+ctx->fill_offset,str,len);
    ctx->fill_offset += len;
}

ERL_NIF_TERM
encode_string(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    encode_ctx ctx;
    ERL_NIF_TERM ret;
    ErlNifBinary bin;
    
    if(!enif_inspect_iolist_as_binary(env, argv[0], &bin))
    {
        ret = enif_make_badarg(env);
        goto done;
    }
    if (!enif_alloc_binary(bin.size + 2, &(ctx.bin)))
    {
        ret =  enif_make_tuple(env, 2,
            enif_make_atom(env, "error"),
            enif_make_atom(env, "insufficient_memory")
        );
        goto done;
    }
    ctx.env = env;
    ctx.fill_offset = 0;
    ctx.fatal_error = 0;
    
    fill_buffer(&ctx,"\"", 1);
    yajl_string_encode2(fill_buffer, &ctx, bin.data, bin.size);
    fill_buffer(&ctx,"\"", 1);
    
    if (ctx.fatal_error) {
        ret =  enif_make_tuple(env, 2,
            enif_make_atom(env, "error"),
            enif_make_atom(env, "insufficient_memory")
            );
    }
    else {
        ret = enif_make_binary(env, &ctx.bin);
    }
done:
    return ret;
}
