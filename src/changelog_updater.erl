%% @copyright 2022 Guilherme Andrade
%%
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy  of this software and associated documentation files (the "Software"),
%% to deal in the Software without restriction, including without limitation
%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%% and/or sell copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%% DEALINGS IN THE SOFTWARE.

-module(changelog_updater).

%%
%% Assumes https://keepachangelog.com/en/1.0.0/ is used
%%

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([insert_addition/2,
         insert_change/2,
         insert_removal/2,
         insert_fix/2]).

-ignore_xref([
         insert_addition/2,
         insert_change/2,
         insert_removal/2,
         insert_fix/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec insert_addition(Addition, Log) -> {ok, UpdatedLog}
    when Addition :: unicode:chardata(),
         Log :: unicode:chardata(),
         UpdatedLog :: unicode:chardata().
insert_addition(Addition, Log) ->
    do_insert_change("Added", Addition, Log).

-spec insert_change(Change, Log) -> {ok, UpdatedLog}
    when Change :: unicode:chardata(),
         Log :: unicode:chardata(),
         UpdatedLog :: unicode:chardata().
insert_change(Change, Log) ->
    do_insert_change("Changed", Change, Log).

-spec insert_removal(Removal, Log) -> {ok, UpdatedLog}
    when Removal :: unicode:chardata(),
         Log :: unicode:chardata(),
         UpdatedLog :: unicode:chardata().
insert_removal(Removal, Log) ->
    do_insert_change("Removed", Removal, Log).

-spec insert_fix(Fix, Log) -> {ok, UpdatedLog}
    when Fix :: unicode:chardata(),
         Log :: unicode:chardata(),
         UpdatedLog :: unicode:chardata().
insert_fix(Fix, Log) ->
    do_insert_change("Fixed", Fix, Log).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

do_insert_change(ChangeType, Change, Log) ->
    <<BinLog/bytes>> = unicode:characters_to_binary(Log),
    UpdatedLog = insert_change(ChangeType, Change, BinLog),
    {ok, UpdatedLog}.

insert_change(ChangeType, Change, Log) ->
    case find_unreleased_version_insertion_point(Log) of
        {new, Offset} ->
            insert_change_for_newly_unreleased_version(ChangeType, Change, Offset, Log);
        {existing, Offset} ->
            insert_change_for_existing_unreleased_version(ChangeType, Change, Offset, Log)
    end.

find_unreleased_version_insertion_point(Log) ->
    case re:run(Log, unreleased_version_header_regex(), [caseless, multiline]) of
        {match, [{UnreleasedLineStart, UnreleasedLineLength} | _]} ->
            {existing, UnreleasedLineStart + UnreleasedLineLength};
        nomatch ->
            find_newly_unreleased_version_insertion_point(Log)
    end.

find_newly_unreleased_version_insertion_point(Log) ->
    case re:run(Log, released_version_header_regex(), [caseless, multiline]) of
        {match, [{LastVersionLineStart, _} | _]} ->
            {new, LastVersionLineStart};
        nomatch ->
            {new, byte_size(Log)}
    end.

unreleased_version_header_regex() ->
    "^## +\\[unreleased\\] *(?:\n\r?)?".

released_version_header_regex() ->
    VersionPartMatch = "[0-9]+",
    io_lib:format("^## +\\[~s\\.~s\\.~s\\.*].*(?:\n\r?)?",
                  [VersionPartMatch, VersionPartMatch, VersionPartMatch]).

insert_change_for_newly_unreleased_version(ChangeType, Change, Offset, Log) ->
    <<Before:Offset/bytes, After/bytes>> = Log,

    [Before,
     io_lib:format("## [Unreleased]\n"
                   "\n"
                   "### ~ts\n" % Changetype
                   "\n"
                   "- ~ts\n" % Change
                   "\n",
                   [ChangeType, Change]),
     After].

insert_change_for_existing_unreleased_version(ChangeType, Change, Offset, Log) ->
    {BeforeUnreleasedVersion, OurSection, AfterUnreleasedVersion}
        = cut_existing_unreleased_version(Offset, Log),
    Regexp = change_type_header_regex(ChangeType),

    [BeforeUnreleasedVersion,
     case re:run(OurSection, Regexp, [caseless, multiline]) of
         {match, [{LineStart, LineLength} | _]} ->
             StartAt = LineStart + LineLength,
             insert_change_for_existing_change_type(Change, StartAt, OurSection);
         nomatch ->
             insert_change_for_new_change_type(ChangeType, Change, OurSection)
     end,
     AfterUnreleasedVersion].

cut_existing_unreleased_version(Offset, Log) ->
    <<Before:Offset/bytes, AfterVersionLine/bytes>> = Log,

    LengthForThisVersionSection
        = case re:run(AfterVersionLine, released_version_header_regex(), [caseless, multiline]) of
              {match, [{UpToHere, _} | _]} ->
                  UpToHere;
              nomatch ->
                  byte_size(AfterVersionLine)
          end,

    <<ThisVersionSection:LengthForThisVersionSection/bytes,
      After/bytes>> = AfterVersionLine,

    {Before, ThisVersionSection, After}.

insert_change_for_existing_change_type(Change, StartAt, OurVersionSection) ->
    {Before, OurChangeTypeSection, After}
        = cut_existing_change_type_section(StartAt, OurVersionSection),

    [Before,
     insert_change_at_end_of_list(OurChangeTypeSection, Change),
     After].

cut_existing_change_type_section(StartAt, OurVersionSection) ->
    <<Before:StartAt/bytes, Remaining/bytes>> = OurVersionSection,

    case re:run(Remaining, any_change_type_header_regex(), [caseless, multiline]) of
        {match, [{NextChangeTypeStartsAt, _}]} ->
            <<OurChangeTypeSection:NextChangeTypeStartsAt/bytes,
              After/bytes>> = Remaining,
            {Before, OurChangeTypeSection, After};
        nomatch ->
            {Before, Remaining, <<>>}
    end.

insert_change_at_end_of_list(ListSection, Change) ->
    case re:run(ListSection, "(?:\n\r?)[\s\n\r]+$", [dollar_endonly]) of
        {match, [{PrevLineEndsPos, _}]} ->
            <<UpUntilPrevLineEnds:PrevLineEndsPos/bytes, PrevLineEndAndMore/bytes>> = ListSection,
            PrevLineEnd = string:slice(PrevLineEndAndMore, 0, 1),
            After = string:slice(PrevLineEndAndMore, 1),
            [UpUntilPrevLineEnds, PrevLineEnd,
             io_lib:format("- ~ts\n", [Change]),
             After];
        nomatch ->
            [ListSection,
             io_lib:format("- ~ts\n", [Change]),
             "\n"]
    end.

insert_change_for_new_change_type(ChangeType, Change, OurVersionSection) ->
    Formatted = io_lib:format("### ~ts\n" % ChangeType
                              "\n"
                              "- ~ts\n" % Change
                              "\n",
                              [ChangeType, Change]),

    SearchResults
        = [re:run(OurVersionSection,
                  change_type_header_regex(ChangeTypeAfter),
                  [caseless, multiline])
           || ChangeTypeAfter <- change_types_listed_after(ChangeType)],

    case [Match || {match, _} = Match <- SearchResults] of
        [{match, [{PosOfNextChange, _} | _]} | _] ->
            <<Before:PosOfNextChange/bytes, After/bytes>> = OurVersionSection,
            [ensure_two_newlines_at_the_end_if_large_enough(Before),
             Formatted,
             After];
        [] ->
            [ensure_two_newlines_at_the_end(OurVersionSection),
             Formatted]
    end.

ensure_two_newlines_at_the_end_if_large_enough(String) ->
    case string:length(String) >= 2 of
        true ->
            ensure_two_newlines_at_the_end(String);
        false ->
            String
    end.

ensure_two_newlines_at_the_end(String) ->
    Len = string:length(String),
    LastTwoGlyphs = string:slice(String, Len - 2, 2),
    AmountOfNewLines = length([G || G <- unicode:characters_to_list(LastTwoGlyphs), is_newline(G)]),
    NewLinesToPut = 2 - AmountOfNewLines,
    [String, string:copies("\n", NewLinesToPut)].

is_newline(Glyph) ->
    lists:member(unicode:characters_to_binary([Glyph]),
                 [<<"\n">>, <<"\n\r">>, <<"\r">>]).

change_types_listed_after(ChangeType) ->
    [_ | After]
        = lists:dropwhile(
            fun (CType) -> CType =/= ChangeType end,
            ["Added", "Changed", "Removed", "Fixed"]),
    After.

change_type_header_regex(ChangeType) ->
    io_lib:format("^### +~ts *(?:\n\r?)?", [ChangeType]).

any_change_type_header_regex() ->
    "^### +\\w+(?:\n\r?)?".
