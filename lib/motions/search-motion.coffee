_ = require 'underscore-plus'
{MotionWithInput} = require './general-motions'
SearchViewModel = require '../view-models/search-view-model'
{Input} = require '../view-models/view-model'
{Point, Range} = require 'atom'
settings = require '../settings'

class SearchBase extends MotionWithInput
  operatesInclusively: false

  constructor: (@editor, @vimState, options = {}) ->
    super(@editor, @vimState)
    @reverse = @initiallyReversed = false
    @updateCurrentSearch() unless options.dontUpdateCurrentSearch

  reversed: =>
    @initiallyReversed = @reverse = true
    @updateCurrentSearch()
    this

  moveCursor: (cursor, count=1) ->
    ranges = @scan(cursor)
    if ranges.length > 0
      range = ranges[(count - 1) % ranges.length]
      cursor.setBufferPosition(range.start)
    else
      atom.beep()

  scan: (cursor) ->
    currentPosition = cursor.getBufferPosition()

    [rangesBefore, rangesAfter] = [[], []]
    @editor.scan @getSearchTerm(@input.characters), ({range}) =>
      isBefore = if @reverse
        range.start.compare(currentPosition) < 0
      else
        range.start.compare(currentPosition) <= 0

      if isBefore
        rangesBefore.push(range)
      else
        rangesAfter.push(range)

    if @reverse
      rangesAfter.concat(rangesBefore).reverse()
    else
      rangesAfter.concat(rangesBefore)

  getSearchTerm: (term) ->
    modifiers = {'g': true}

    if not term.match('[A-Z]') and settings.useSmartcaseForSearch()
      modifiers['i'] = true

    if term.indexOf('\\c') >= 0
      term = term.replace('\\c', '')
      modifiers['i'] = true

    modFlags = Object.keys(modifiers).join('')

    try
      new RegExp(term, modFlags)
    catch
      new RegExp(_.escapeRegExp(term), modFlags)

  updateCurrentSearch: ->
    @vimState.globalVimState.currentSearch.reverse = @reverse
    @vimState.globalVimState.currentSearch.initiallyReversed = @initiallyReversed

  replicateCurrentSearch: ->
    @reverse = @vimState.globalVimState.currentSearch.reverse
    @initiallyReversed = @vimState.globalVimState.currentSearch.initiallyReversed

class Search extends SearchBase
  constructor: (@editor, @vimState) ->
    super(@editor, @vimState)
    @viewModel = new SearchViewModel(this)

class SearchCurrentWord extends SearchBase
  @keywordRegex: null

  constructor: (@editor, @vimState) ->
    super(@editor, @vimState)

    # FIXME: This must depend on the current language
    defaultIsKeyword = "[@a-zA-Z0-9_\-]+"
    userIsKeyword = atom.config.get('vim-mode.iskeyword')
    @keywordRegex = new RegExp(userIsKeyword or defaultIsKeyword)

    searchString = @getCurrentWordMatch()
    @input = new Input(searchString)
    @vimState.pushSearchHistory(searchString) unless searchString is @vimState.getSearchHistoryItem()

  getCurrentWord: ->
    cursor = @editor.getLastCursor()
    wordStart = cursor.getBeginningOfCurrentWordBufferPosition(wordRegex: @keywordRegex, allowPrevious: false)
    wordEnd   = cursor.getEndOfCurrentWordBufferPosition      (wordRegex: @keywordRegex, allowNext: false)
    cursorPosition = cursor.getBufferPosition()

    if wordEnd.column is cursorPosition.column
      # either we don't have a current word, or it ends on cursor, i.e. precedes it, so look for the next one
      wordEnd = cursor.getEndOfCurrentWordBufferPosition      (wordRegex: @keywordRegex, allowNext: true)
      return "" if wordEnd.row isnt cursorPosition.row # don't look beyond the current line

      cursor.setBufferPosition wordEnd
      wordStart = cursor.getBeginningOfCurrentWordBufferPosition(wordRegex: @keywordRegex, allowPrevious: false)

    cursor.setBufferPosition wordStart

    @editor.getTextInBufferRange([wordStart, wordEnd])

  cursorIsOnEOF: (cursor) ->
    pos = cursor.getNextWordBoundaryBufferPosition(wordRegex: @keywordRegex)
    eofPos = @editor.getEofBufferPosition()
    pos.row is eofPos.row and pos.column is eofPos.column

  getCurrentWordMatch: ->
    characters = @getCurrentWord()
    if characters.length > 0
      if /\W/.test(characters) then "#{characters}\\b" else "\\b#{characters}\\b"
    else
      characters

  isComplete: -> true

  execute: (count=1) ->
    super(count) if @input.characters.length > 0

OpenBrackets = ['(', '{', '[', '/*']
CloseBrackets = [')', '}', ']', '*/']
AnyBracket = new RegExp(OpenBrackets.concat(CloseBrackets).map(_.escapeRegExp).join("|"))

###
  Refactoring the Bracket Matcher system:

  This should support any encapsulation types necessary for:
    {}, (), [], block comments, #if #else #endif
  It should also be able to support fuzzy 'searching' for things
  like methods and classes.

  It'll be tougher to match up the if/else/endif because of the
  odd structure of having #else be both open and close.

  It will also need to be able to detect the type of element the cursor
  is currently on top of, even if it is a 3-char string and the cursor is on
  the very last matching character (#endi|f)

  The structure for method/class is:
    class foo {
      int method_one() {
        body_one();
      }
      int method_two() {
        body_two();
      }
    }

  The methods should be able to search for any given start or
  end pattern, and adding support for new patterns should be
  simple as registering the open and close versions and then
  telling hotkeys whether they should look for the closers in front,
  or openers behind. If no particular search group is given,
  there is a discovery system that parses through registered encapsulation
  objects, looking to see if the cursor sits on a match.

  As we scan from the starting point, we should look for any
  instances of the starters or closers. The goal being that when
  the given count reaches a certain value, we've encountered enough
  opening or closing tags that there is an unfinished one.
  | () ) <- needs to see the open and then close, then see one more close. [+1, -1, -1] => -1 (starts +)
  ( () | <- needs to see the close and then open, then see one more open. [-1, +1, -1] => -1 (starts -)

  if everything is regular expressions, then we could read in a whole line,
  scan for matches via regex on that string (up to the current cursor position),
  and sort through them depending on direction (reversed if looking for openers).
  This might be less performant for longer lines, but probably not for longer patterns.
###

class EncapsulationGroup
  constructor: (@opener, @closer) ->
    @opener = new RegExp @opener if typeof @opener is 'string'
    @closer = new RegExp @closer if typeof @closer is 'string'
    @combined = new RegExp "(#{@opener.source})|(#{@closer.source})", 'g'

    @opener = new RegExp @opener.source, 'g' if not @opener.global
    @closer = new RegExp @closer.source, 'g' if not @closer.global

encapsulationGroups =
  curlyBrackets: new EncapsulationGroup /\{/,        /\}/            # curly brackets
  parenthesis:   new EncapsulationGroup /\(/,        /\)/            # parenthesis
  brackets:      new EncapsulationGroup /\[/,        /\]/            # brackets
  comments:      new EncapsulationGroup /\/\*/,      /\*\//          # multiline comments
  conditions:    new EncapsulationGroup /#if|#else/, /#else|#endif/  # compiler conditions


class BracketMatchingMotion extends SearchBase
  isComplete: -> true

  constructor: (@editor, @vimState, @operatesInclusively = true, encapsulationGroupId = null, searchForCloser = null) ->
    if encapsulationGroupId isnt null
      @searchGroup = encapsulationGroups[@encapsulationGroupId]
      @searchDirection = if searchForCloser then 1 else -1
      @searchFor = if searchForCloser then 'closer' else 'opener'

  # TODO: Refactor this. It's pretty awkward...
  getGroupFromCursor: (cursor) ->
    # find what group matches (if any) the current cursor's position
    searchRange = cursor.getCurrentLineBufferRange()
    cursorPoint = cursor.getBufferPosition()
    cursorRange = [cursor.getBufferPosition(), cursor.getBufferPosition().translate([0, 1])]
    closestMatch = null

    scanHelper = (group, encapType, onComplete) ->
      ({range, stop}) ->
        potentialMatch = range: range, type: encapType, group: group
        match = potentialClosest = null
        if range.containsRange cursorRange
          match = potentialMatch
          stop()
        else if range.start.isGreaterThan(cursorPoint)
          potentialClosest = potentialMatch
        onComplete match, potentialClosest

    searchTypes = ['opener', 'closer']
    for groupName, group of encapsulationGroups

      # console.log "scanning '#{@editor.getTextInBufferRange(searchRange)}' for", groupName, group

      for type in searchTypes
        match = null
        closestPotential = null
        @editor.scanInBufferRange group[type], searchRange, scanHelper group, type, (oMatch, oClosestMatch) ->
          match = oMatch
          closestPotential = oClosestMatch

        return match if match?
        closestMatch = closestPotential if not closestMatch?
        closestMatch = closestPotential if closestPotential?.range.start.column < closestMatch?.range.start.column

    return closestMatch if closestMatch?
    return range: null, group: null, type: null

  scanFor: (startPosition) ->
    result = null
    cursorRange = [startPosition, startPosition.translate([0, 1])]
    scanMethod = if @searchFor is 'closer' then 'scanInBufferRange' else 'backwardsScanInBufferRange'
    eofPosition = @editor.getEofBufferPosition().translate([0, 1])
    points = [startPosition]
    points.unshift [0, -1] if @searchFor is 'opener'
    points.push eofPosition if @searchFor is 'closer'

    # console.log 'Scanning:', @searchFor, @searchGroup, scanMethod
    # console.log @editor.getTextInBufferRange points

    # from here we have a range to search through, the group to look for,
    # the specific expression(s) to use, and what exactly we want to find.
    currentCount = 0
    @editor[scanMethod] @searchGroup.combined, points, ({match, range, stop}) =>
      return if range.containsRange cursorRange
      openMatch = match[1]
      closeMatch = match[2]
      currentCount++ if openMatch
      currentCount-- if closeMatch
      if (@searchFor is 'opener' and currentCount > 0) or (@searchFor is 'closer' and currentCount < 0)
        result = if @searchFor is 'opener' then range.start else range.end.translate([0, -1])
        stop()

    result

  moveCursor: (cursor) ->
    if not @searchGroup?
      {range, type, group} = @getGroupFromCursor cursor
      @searchGroup = group
      @searchDirection = if type is 'opener' then 1 else -1
      @searchFor = if type is 'opener' then 'closer' else 'opener'
      cursor.setBufferPosition range.start if range?
    return cursor.getBufferPosition() if not @searchGroup?

    # if I have results here, then they represent the current
    # element the cursor is on. We now need to seek out its
    # counterpart.
    if position = @scanFor cursor.getBufferPosition()
      cursor.setBufferPosition position


class BracketMatchingMotion2 extends SearchBase
  operatesInclusively: true
  invertSearchDirection: false

  isComplete: -> true

  searchCompleteCondition: (point, depth, character) ->
    return point if depth is 0
    return null if depth < 0

  searchForMatch: (startPosition, reverse, inCharacter, outCharacter) ->
    depth = 0
    point = startPosition.copy()
    lineLength = @editor.lineTextForBufferRow(point.row).length
    eofPosition = @editor.getEofBufferPosition().translate([0, 1])
    increment = if reverse then -1 else 1
    characterLen = inCharacter.length
    index = 0

    console.log inCharacter, outCharacter

    loop
      character = @characterAt(point, characterLen)

      if @operatesInclusively or index >= characterLen
        depth++ if character is inCharacter
        depth-- if character is outCharacter

      condition = @searchCompleteCondition(point, depth, character)
      return condition if condition isnt undefined

      point.column += increment
      return null if point.isEqual([0, -1])
      return null if point.isEqual(eofPosition)

      if point.column < 0
        point.row--
        lineLength = @editor.lineTextForBufferRow(point.row)?.length
        point.column = lineLength - 1
      else if point.column >= lineLength
        point.row++
        lineLength = @editor.lineTextForBufferRow(point.row)?.length
        point.column = 0

      return null if lineLength is undefined
      index++

  characterAt: (position, length = 1) ->
    @editor.getTextInBufferRange([position, position.translate([0, length])])

  getSearchData: (position) ->
    brackets = OpenBrackets.concat CloseBrackets
    _.chain(brackets)
      .map (bracket) -> bracket.length
      .uniq()
      .reduce (match, len) =>
        # this should check as if I am at odd ends or the middle of the current
        # thing we are searching for
        console.log [position.column - len + 1..position.column]
        # for offset in [position.column - len..position.column + len]
        checkMatch = @characterAt(position, len)
        return checkMatch if brackets.indexOf(checkMatch) >= 0
        return match
      , null
      .value()

  getBracketMatch: (character) ->
    if (index = OpenBrackets.indexOf(character)) >= 0
      [character, CloseBrackets[index], if @invertSearchDirection then true else false]
    else if (index = CloseBrackets.indexOf(character)) >= 0
      [character, OpenBrackets[index], if @invertSearchDirection then false else true]
    else
      []

  moveCursor: (cursor) ->
    startPosition = cursor.getBufferPosition()

    [inCharacter, outCharacter, reverse] = @getBracketMatch @getSearchData(startPosition)

    unless inCharacter?
      restOfLine = [startPosition, [startPosition.row, Infinity]]
      @editor.scanInBufferRange AnyBracket, restOfLine, ({range, stop}) ->
        startPosition = range.start
        stop()

      [inCharacter, outCharacter, reverse] = @getBracketMatch @getSearchData(startPosition)

    return unless inCharacter?

    if matchPosition = @searchForMatch(startPosition, reverse, inCharacter, outCharacter)
      # console.log "matching #{outCharacter} #{CloseBrackets.indexOf(outCharacter)}"
      if CloseBrackets.indexOf(outCharacter) >= 0
        matchPosition = matchPosition.translate([0, inCharacter.length - 1])
      cursor.setBufferPosition(matchPosition)

class RepeatSearch extends SearchBase
  constructor: (@editor, @vimState) ->
    super(@editor, @vimState, dontUpdateCurrentSearch: true)
    @input = new Input(@vimState.getSearchHistoryItem(0) ? "")
    @replicateCurrentSearch()

  isComplete: -> true

  reversed: ->
    @reverse = not @initiallyReversed
    this

###
  Search for a given bracket instead of just the character we are on
###
class BracketSearchingMotion extends BracketMatchingMotion2
  operatesInclusively: false
  invertSearchDirection: true

  constructor: (@editor, @vimState, @character) -> super(@editor, @vimState)

  getSearchData: () -> @character

  searchCompleteCondition: (point, depth, character) -> return point if depth > 0

module.exports = {Search, SearchCurrentWord, BracketMatchingMotion, RepeatSearch, BracketSearchingMotion}
