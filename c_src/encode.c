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
    memcpy(ctx->bin.data + ctx->fill_offset, str, len);
    ctx->fill_offset += len;
}


static int
encode_string(ErlNifEnv* env, ERL_NIF_TERM binary, ERL_NIF_TERM *pterm)
{
    encode_ctx ctx;
    ErlNifBinary bin;
    
    if(!enif_inspect_binary(env, binary, &bin))
    {
        return 0;
    }
    if (!enif_alloc_binary(bin.size + 2, &(ctx.bin)))
    {
        return 0;
    }
    ctx.env = env;
    ctx.fill_offset = 0;
    ctx.fatal_error = 0;
    fill_buffer(&ctx, "\"", 1);
    yajl_string_encode2(fill_buffer, &ctx, bin.data, bin.size);
    fill_buffer(&ctx, "\"", 1);
    
    if (ctx.fatal_error) {
        return 0;
    }
     /* size might be bigger than the fill offset,
     make smaller to truncate the garbage. */
    ctx.bin.size = ctx.fill_offset;
    *pterm = enif_make_binary(env, &ctx.bin);
    return 1;
}

ERL_NIF_TERM
final_encode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#define BADARG ret = enif_make_badarg(env); goto done;

    ERL_NIF_TERM ret = enif_make_list_from_array(env, NULL, 0);
    ERL_NIF_TERM head = argv[0];
    ERL_NIF_TERM term;
    double number;
    char buffer[32];
    while(enif_get_list_cell(env, head, &term, &head)) {
        //We scan the list, looking for things to encode. Most items we
        // output untouched, but tuples we be tagged with a type and a 
        //value: {Type, Value} where Type is a an Integer.
        
        if (enif_is_tuple(env, term)) {
            // It's a tuple.
            const ERL_NIF_TERM* array;
            int arity;
            int code;
            if (!enif_get_tuple(env, term, &arity, &array)) {
                ret =  enif_make_tuple(env, 2,
                    enif_make_atom(env, "error"),
                    enif_make_atom(env, "invalid_number")
                );
            }
            if (arity != 2 || !enif_get_int(env, array[0], &code)) {
                BADARG;
            }
            if (code == 0) {
                // {0, String}
                if (!encode_string(env, array[1], &term)) {
                    BADARG;
                }
            }
            else {
                // {1, Double}
                if(!enif_get_double(env, array[1], &number)) {
                    BADARG;
                }
                if (isnan(number) || isinf(number)) {
                    BADARG;
                }
                snprintf(buffer, sizeof(buffer), "%.16g", number);
                term = enif_make_string(env, buffer, ERL_NIF_LATIN1);
            }
        }
        // Now encode the term into a new list head
        ret = enif_make_list_cell(env, term, ret);
    }
done:
    return ret;
}

