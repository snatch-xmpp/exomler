-module(exomler).

%% API
-export([decode/1, decode/2]).
-export([decode_document/1, decode_document/2]).
-export([encode/1, encode_document/1]).

-type xml_document() :: binary().
-type xml_text() :: binary().
-type attrs() :: [{binary(), binary()}].
-type content() :: [binary() | xml_ast()].
-type xml_ast() :: {Tag :: binary(), attrs(), content()}.
-type xml_version() :: '1.0' | '1.1'.
-type xml_encoding() :: utf8 | latin1.
-type xml_term() :: {xml, xml_version(), xml_encoding(), xml_ast()}.

%% API
-spec decode_document(XmlDoc :: xml_document()) -> xml_term().
decode_document(Bin) ->
    decode_document(Bin, #{}).

decode_document(XmlDoc, Opts) ->
    exomler_dom_decoder:decode_document(XmlDoc, Opts).

-spec encode_document(Term :: xml_term()) -> xml_document().
encode_document(Term) ->
    exomler_dom_encoder:encode_document(Term).

-spec decode(XML :: xml_text()) -> xml_ast().
decode(XML) ->
    exomler_dom_decoder:decode(XML, #{}).

decode(XML, Opts) ->
    exomler_dom_decoder:decode(XML, Opts).

-spec encode(Ast :: xml_ast()) -> xml_text().
encode(Ast) ->
    exomler_dom_encoder:encode(Ast).
