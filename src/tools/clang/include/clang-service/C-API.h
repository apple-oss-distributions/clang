#ifndef LLVM_CLANG_CLANGSERVICE_C_API_H
#define LLVM_CLANG_CLANGSERVICE_C_API_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

/**
 * \brief Create and inspect resource identifiers.
 */

typedef void *CXSUID;

/*
 * Create a resource identifier from a null-terminated string.
 */
CXSUID CXSUID_create(const char *uid);

/*
 * Get a null-terminated string which contains a human-readable version of the
 * resource identifier keyed by this UID.
 */
const char *CXSUID_get_name(CXSUID cxs_uid);


/**
 * \brief Create, inspect, and free messages.
 *
 * Messages to and from the clang service are represented as CXSValues.
 *
 * Functions with names ending in "create" return CXSValues which need to be
 * freed. Getter functions typically do not return CXSValues: the ones which
 * do retain ownership of the returned values.
 */

typedef struct {
  void *opaque1;
  unsigned opaque2[2];
} CXSValue;

enum CXSValue_Kind {
  CXSValue_Null,
  CXSValue_Bool,
  CXSValue_Int64,
  CXSValue_UID,
  CXSValue_String,
  CXSValue_Data,
  CXSValue_Array,
  CXSValue_Dict
};

/*
 * Create a value of kind CXSValue_Null.
 *
 * The returned value is owned by the caller and must be freed.
 */
CXSValue CXSValue_null_create(void);

/*
 * Create a value of kind CXSValue_Bool.
 *
 * The returned value is owned by the caller and must be freed.
 */
CXSValue CXSValue_bool_create(bool value);

bool CXSValue_bool_get_value(CXSValue xbool);

/*
 * Create a value of kind CXSValue_Int64.
 *
 * The returned value is owned by the caller and must be freed.
 */
CXSValue CXSValue_int64_create(int64_t value);

int64_t CXSValue_int64_get_value(CXSValue xint);

/*
 * Create a value of kind CXSValue_UID.
 *
 * The returned value is owned by the caller and must be freed.
 */
CXSValue CXSValue_uid_create(CXSUID cxs_uid);

CXSUID CXSValue_uid_get_value(CXSValue xuid);

/*
 * Create a value of kind CXSValue_String.
 *
 * \p string must be a null-terminated string.
 *
 * The returned value is owned by the caller and must be freed.
 *
 * If \p no_copy is true, it is assumed that the lifetime of \p string is at
 * least as long as the lifetime of the returned value. Therefore \p string is
 * not copied.
 *
 * If \p no_copy is false, \p string is copied into memory owned by the
 * returned value.
 */
CXSValue CXSValue_string_create(const char *string, bool no_copy);

size_t CXSValue_string_get_length(CXSValue xstring);

/*
 * Get the raw string pointer owned by a value.
 *
 * The lifetime of the returned pointer is tied to the lifetime of \p xstring.
 */
const char *CXSValue_string_get_string_ptr(CXSValue xstring);

/*
 * Create a value of kind CXSValue_Data.
 *
 * The returned value is owned by the caller and must be freed.
 *
 * If \p no_copy is true, it is assumed that the lifetime of \p bytes is at
 * least as long as the lifetime of the returned value. Therefore \p bytes is
 * not copied.
 *
 * If \p no_copy is false, \p bytes is copied into memory owned by the returned
 * value.
 */
CXSValue CXSValue_data_create(char *bytes, size_t length, bool no_copy);

size_t CXSValue_data_get_length(CXSValue xdata);

/*
 * Get the raw data pointer owned by a value.
 *
 * The lifetime of the returned pointer is tied to the lifetime of \p xdata.
 */
const char *CXSValue_data_get_bytes_ptr(CXSValue xdata);

/*
 * Create a value of kind CXSValue_Array.
 *
 * The returned value is owned by the caller and must be freed.
 */
CXSValue CXSValue_array_create(void);

/*
 * Append a value to \p array.
 *
 * \p array takes ownership of the value pointer-to by \p value. When this
 * function returns, \p value points to a null value.
 */
void CXSValue_array_append_value(CXSValue array, CXSValue *value);

/*
 * Get a value from \p array.
 *
 * \p array retains ownership of the returned value. It should not be freed.
 */
CXSValue CXSValue_array_get_value(CXSValue array, size_t index);

size_t CXSValue_array_get_count(CXSValue array);

/*
 * Create a value of kind CXSValue_Dict.
 *
 * The returned value is owned by the caller and must be freed.
 */
CXSValue CXSValue_dictionary_create(void);

/*
 * Set a value in \p dictionary.
 *
 * \p dictionary takes ownership of the value pointer-to by \p value. When this
 * function returns, \p value points to a null value.
 */
void CXSValue_dictionary_set_value(CXSValue dictionary, CXSUID key,
                                   CXSValue *value);

/*
 * Get a value from \p dictionary.
 *
 * \p dictionary retains ownership of the returned value. It should not be
 * freed.
 */
CXSValue CXSValue_dictionary_get_value(CXSValue dictionary, CXSUID key);

size_t CXSValue_dictionary_get_count(CXSValue dictionary);

/*
 * Functions of this type may be mapped over the key-value pairs in a
 * dictionary. A `false` return value terminates iteration.
 */
typedef bool (*CXSValue_dictionary_applier_t)(CXSUID key,
                                              CXSValue cxs_value);

void CXSValue_dictionary_apply(CXSValue dictionary,
                               CXSValue_dictionary_applier_t applier);

enum CXSValue_Kind CXSValue_get_kind(CXSValue cxs_value);

/*
 * Release ownership of a value and free any memory associated with it. When
 * this function returns, \p cxs_value points to a null value.
 */
void CXSValue_free(CXSValue *cxs_value);


/**
 * \brief Create and free Clients.
 *
 * Clients are used to communicate with the clang service.
 */

typedef void *CXSClient;

enum CXSClient_Kind {
  CXSClient_InProcess,
  CXSClient_XPC
};

/*
 * Create a client with the specified client kind.
 *
 * The client should eventually be freed.
 */
CXSClient CXSClient_create(enum CXSClient_Kind client_kind);

/*
 * The client takes ownership of the request.
 *
 * The returned value must be freed.
 */
CXSValue CXSClient_request(CXSClient client, CXSValue *request);

/*
 * The client takes ownership of the request.
 *
 * The value passed to the handler must be freed.
 */
typedef void (*CXSClient_handler_t)(CXSValue response);

void CXSClient_request_async(CXSClient client, CXSValue *request,
                             CXSClient_handler_t handler);

/*
 * Tear down and free \p client.
 */
void CXSClient_free(CXSClient client);

#ifdef __cplusplus
} // end extern "C"
#endif

#endif // LLVM_CLANG_CLANGSERVICE_C_API_H
