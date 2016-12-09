#ifndef REGISTER_UID
#error Missing definition of REGISTER_UID(Name, UIDStr).
#endif

/* Generic key UIDs. */
REGISTER_UID(KeyRequest, "key.request")
REGISTER_UID(KeyError, "key.error")
REGISTER_UID(KeyToken, "key.token")
REGISTER_UID(KeyLHSToken, "key.lhs_token")
REGISTER_UID(KeyRHSToken, "key.rhs_token")
REGISTER_UID(KeyManager, "key.manager")
REGISTER_UID(KeyFile, "key.loc.file")
REGISTER_UID(KeySourceLoc, "key.loc")
REGISTER_UID(KeyModule, "key.loc.module")
REGISTER_UID(KeyName, "key.loc.name")
REGISTER_UID(KeyLineOffset, "key.loc.line")
REGISTER_UID(KeyColOffset, "key.loc.column")
REGISTER_UID(KeyOffset, "key.loc.offset")
REGISTER_UID(KeyRangeStart, "key.range.start")
REGISTER_UID(KeyRangeEnd, "key.range.end")
REGISTER_UID(KeyCmdArgs, "key.cmd.args")
REGISTER_UID(KeyFiles, "key.files")
REGISTER_UID(KeyBuffer, "key.buffer")
REGISTER_UID(KeySort, "key.sort")
REGISTER_UID(KeyParseOptions, "key.parse.options")

/* Miscellaneous key UIDs. */
REGISTER_UID(KeyVersionMajor, "key.version_major")
REGISTER_UID(KeyVersionMinor, "key.version_minor")
REGISTER_UID(KeyIndexOptions, "key.index.options")
REGISTER_UID(KeyIndexExcludeDeclsFromPCH, "key.index.exclude_decls_pch")
REGISTER_UID(KeyIndexDiagnostics, "key.index.diagnostics")

/* Diagnostics key UIDs. */
REGISTER_UID(KeyDiagnostics, "key.diagnostics")
REGISTER_UID(KeyDiagnosticsEnabled, "key.diagnostics.enabled")
REGISTER_UID(KeyDiagnosticsOptions, "key.diagnostics.options")
REGISTER_UID(KeyDiagnosticSeverity, "key.diagnostic.severity")
REGISTER_UID(KeyDiagnosticString, "key.diagnostic.string")
REGISTER_UID(KeyDiagnosticLocation, "key.diagnostic.location")
REGISTER_UID(KeyDiagnosticFixits, "key.diagnostic.fixits")

/* Code completion key UIDs. */
REGISTER_UID(KeyCodeCompleteOptions, "key.codecomplete.options")
REGISTER_UID(KeyCodeCompleteResults, "key.codecomplete.results")
REGISTER_UID(KeyCodeCompleteContexts, "key.codecomplete.contexts")
REGISTER_UID(KeyCodeCompleteContainerKind, "key.codecomplete.container.kind")
REGISTER_UID(KeyCodeCompleteContainerIncomplete,
             "key.codecomplete.container.incomplete")
REGISTER_UID(KeyCodeCompleteContainerUSR, "key.codecomplete.container.usr")
REGISTER_UID(KeyCodeCompleteObjCSelector, "key.codecomplete.objc_selector")
REGISTER_UID(KeyCodeCompleteCompletionKind, "key.codecomplete.completion.kind")
REGISTER_UID(KeyCodeCompleteCompletionStr, "key.codecomplete.completion.str")
REGISTER_UID(KeyCodeCompleteCompletionStrKind,
             "key.codecomplete.completion.str.kind")
REGISTER_UID(KeyCodeCompleteCompletionStrText,
             "key.codecomplete.completion.str.text")
REGISTER_UID(KeyCodeCompleteCompletionStrChunks,
             "key.codecomplete.completion.str.chunks")
REGISTER_UID(KeyCodeCompleteCompletionStrAvailability,
             "key.codecomplete.completion.str.availability")
REGISTER_UID(KeyCodeCompleteCompletionStrPriority,
             "key.codecomplete.completion.str.priority")
REGISTER_UID(KeyCodeCompleteCompletionStrAnnotations,
             "key.codecomplete.completion.str.annotations")
REGISTER_UID(KeyCodeCompleteCompletionStrParent,
             "key.codecomplete.completion.str.parent")
REGISTER_UID(KeyCodeCompleteCompletionStrParentKind,
             "key.codecomplete.completion.str.parent.kind")
REGISTER_UID(KeyCodeCompleteCompletionStrBrief,
             "key.codecomplete.completion.str.brief")

/* Indexing key UIDs. */
REGISTER_UID(KeyIndexingOptions, "key.indexing.options")
REGISTER_UID(KeyIndexingStatus, "key.indexing.status")
REGISTER_UID(KeyIndexingDoAbort, "key.indexing.do_abort")

REGISTER_UID(KeyIdxIncludedFileInfo, "key.indexing.included_file_info")
REGISTER_UID(KeyIdxImportedASTFileInfo, "key.indexing.imported_ast_file_info")
REGISTER_UID(KeyIdxDeclInfo, "key.indexing.decl_info")
REGISTER_UID(KeyIdxEntityRefInfo, "key.indexing.entity_ref_info")
REGISTER_UID(KeyIdxLoc, "key.indexing.loc")
REGISTER_UID(KeyIdxClientFile, "key.indexing.client_file")
REGISTER_UID(KeyIdxClientASTFile, "key.indexing.client_ast_file")
REGISTER_UID(KeyIdxClientContainer, "key.indexing.client_container")
REGISTER_UID(KeyIdxIsImport, "key.indexing.is_import")
REGISTER_UID(KeyIdxIsAngled, "key.indexing.is_angled")
REGISTER_UID(KeyIdxIsModuleImport, "key.indexing.is_module_import")
REGISTER_UID(KeyIdxIsImplicit, "key.indexing.is_implicit")
REGISTER_UID(KeyIdxEntityInfo, "key.indexing.entity_info")
REGISTER_UID(KeyIdxEntityInfoParent, "key.indexing.entity_info.parent")
REGISTER_UID(KeyIdxContainerSemInfo, "key.indexing.sem.container_info")
REGISTER_UID(KeyIdxContainerLexInfo, "key.indexing.lex.container_info")
REGISTER_UID(KeyIdxIsRedeclaration, "key.indexing.is_redeclaration")
REGISTER_UID(KeyIdxIsDefinition, "key.indexing.is_definition")
REGISTER_UID(KeyIdxIsContainer, "key.indexing.is_container")
REGISTER_UID(KeyIdxContainerDeclInfo, "key.indexing.decl.container_info")
REGISTER_UID(KeyIdxAttrInfos, "key.indexing.attr_infos")
REGISTER_UID(KeyIdxDeclInfoFlags, "key.indexing.decl_info_flags")
REGISTER_UID(KeyIdxEntityRefKind, "key.indexing.entity_ref_kind")
REGISTER_UID(KeyIdxAttrKind, "key.indexing.attr_kind")
REGISTER_UID(KeyIdxEntityKind, "key.indexing.entity_kind")
REGISTER_UID(KeyIdxEntityCXXTemplateKind,
             "key.indexing.entity_cxx_template_kind")
REGISTER_UID(KeyIdxEntityLanguage, "key.indexing.entity_lang")
REGISTER_UID(KeyIdxEntityName, "key.indexing.entity_name")
REGISTER_UID(KeyIdxEntityUSR, "key.indexing.entity_usr")

/* Cursor key UIDs. */
REGISTER_UID(KeyCursor, "key.loc.cursor")
REGISTER_UID(KeyCursorKind, "key.loc.cursor.kind")

/* The request UIDs. */
REGISTER_UID(RequestFileComparison, "source.request.file_comparison")
REGISTER_UID(RequestSourceLocationSpelling, "source.request.loc.spelling")

REGISTER_UID(RequestCodeCompleteOpen, "source.request.codecomplete.open")
REGISTER_UID(RequestCodeCompleteClose, "source.request.codecomplete.close")
REGISTER_UID(RequestCodeCompleteUpdate, "source.request.codecomplete.update")

REGISTER_UID(RequestIndex, "source.request.index")
REGISTER_UID(RequestIndexContinue, "source.request.index.continue")
REGISTER_UID(RequestIndexClose, "source.request.index.close")

REGISTER_UID(RequestVersion, "source.request.version")

#undef REGISTER_UID
