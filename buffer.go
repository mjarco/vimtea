// Package vimtea provides a Vim-like text editor component for terminal applications
package vimtea

import (
	"slices"
	"strings"

	tea "github.com/charmbracelet/bubbletea"
)

// Buffer defines the interface for text buffer operations
// It provides methods for manipulating text content and undo/redo functionality
type Buffer interface {
	// Text returns the entire buffer content as a string
	Text() string

	// Lines returns all lines in the buffer as a string slice
	Lines() []string

	// LineCount returns the number of lines in the buffer
	LineCount() int

	// LineLength returns the length of the line at the given row
	LineLength(row int) int

	// VisualLineLength returns the visual length of the line at the given row
	// counting tabs as tabWidth spaces
	VisualLineLength(row int) int

	// InsertAt inserts text at the specified position
	InsertAt(row, col int, text string)

	// DeleteAt deletes text between the specified positions
	DeleteAt(startRow, startCol, endRow, endCol int)

	// Undo reverts the last change and returns a command with the new cursor position
	Undo() tea.Cmd

	// Redo reapplies a previously undone change
	Redo() tea.Cmd

	// CanUndo returns whether there are changes that can be undone
	CanUndo() bool

	// CanRedo returns whether there are changes that can be redone
	CanRedo() bool

	// Clear removes all content from the buffer and resets to empty state
	Clear() tea.Cmd
}

// buffer implements the Buffer interface
type buffer struct {
	lines     [][]rune      // Text content as lines (each line is a slice of runes)
	undoStack []bufferState // Stack of previous buffer states for undo
	redoStack []bufferState // Stack of undone states for redo
}

// bufferState represents a snapshot of the buffer for undo/redo
type bufferState struct {
	lines  [][]rune // Content at the time of snapshot
	cursor Cursor   // Cursor position at the time of snapshot
}

// TextRange represents a range of text with start and end positions
type TextRange struct {
	Start Cursor // Starting position (inclusive)
	End   Cursor // Ending position (inclusive)
}

// tabWidth defines the visual width of a tab character
const tabWidth = 4

// newBuffer creates a new buffer with the given content
func newBuffer(content string) *buffer {
	stringLines := strings.Split(content, "\n")
	// Convert each line from string to []rune
	lines := make([][]rune, len(stringLines))
	for i, line := range stringLines {
		lines[i] = []rune(line)
	}
	return &buffer{
		lines:     lines,
		undoStack: []bufferState{},
		redoStack: []bufferState{},
	}
}

// text returns the entire buffer content as a string
func (b *buffer) text() string {
	// Convert each line from []rune to string
	stringLines := make([]string, len(b.lines))
	for i, line := range b.lines {
		stringLines[i] = string(line)
	}
	return strings.Join(stringLines, "\n")
}

// lineCount returns the number of lines in the buffer
func (b *buffer) lineCount() int {
	return len(b.lines)
}

// Line returns the content of the line at the given index as a string
// Returns an empty string if the index is out of bounds
func (b *buffer) Line(idx int) string {
	if idx < 0 || idx >= len(b.lines) {
		return ""
	}
	return string(b.lines[idx])
}

// lineRunes returns the content of the line at the given index as []rune
// Returns nil if the index is out of bounds
func (b *buffer) lineRunes(idx int) []rune {
	if idx < 0 || idx >= len(b.lines) {
		return nil
	}
	return b.lines[idx]
}

// lineLength returns the length of the line at the given index
// Returns 0 if the index is out of bounds
func (b *buffer) lineLength(idx int) int {
	if idx < 0 || idx >= len(b.lines) {
		return 0
	}
	return len(b.lines[idx])
}

// visualLineLength returns the visual length of the line, counting tabs as tabWidth spaces
// Returns 0 if the index is out of bounds
func (b *buffer) visualLineLength(idx int) int {
	if idx < 0 || idx >= len(b.lines) {
		return 0
	}
	return visualLength(string(b.lines[idx]), 0)
}

// setLine replaces the line at the given index with new content
// Does nothing if the index is out of bounds
func (b *buffer) setLine(idx int, content string) {
	if idx < 0 || idx >= len(b.lines) {
		return
	}
	b.lines[idx] = []rune(content)
}

// insertLine inserts a new line at the given index
// Does nothing if the index is invalid
func (b *buffer) insertLine(idx int, content string) {
	if idx < 0 || idx > len(b.lines) {
		return
	}

	// Special case: appending at the end
	if idx == len(b.lines) {
		b.lines = append(b.lines, []rune(content))
		return
	}

	// Insert line in the middle
	b.lines = slices.Insert(
		b.lines,
		idx,
		[]rune(content),
	)
}

// deleteLine removes the line at the given index and returns its content
// If it's the last line, clears it instead of removing it
// Returns empty string if the index is out of bounds
func (b *buffer) deleteLine(idx int) string {
	if idx < 0 || idx >= len(b.lines) {
		return ""
	}

	line := string(b.lines[idx])

	// Keep at least one line in the buffer
	if len(b.lines) > 1 {
		b.lines = slices.Delete(b.lines, idx, idx+1)
	} else {
		b.lines[0] = []rune("")
	}

	return line
}

// clear removes all content from the buffer and resets to a single empty line
func (b *buffer) clear() {
	b.lines = [][]rune{[]rune("")}
}

// insertAt inserts text at the specified position
// Handles both single line and multiline inserts
func (b *buffer) insertAt(row, col int, text string) {
	if row < 0 || row >= len(b.lines) {
		return
	}

	line := b.lines[row]
	if col < 0 || col > len(line) {
		return
	}

	stringLines := strings.Split(text, "\n")
	if len(stringLines) == 1 {
		// Simple case: inserting text within a single line
		textRunes := []rune(text)
		newLine := make([]rune, 0, len(line)+len(textRunes))
		newLine = append(newLine, line[:col]...)
		newLine = append(newLine, textRunes...)
		newLine = append(newLine, line[col:]...)
		b.lines[row] = newLine
	} else {
		// Complex case: inserting multiple lines
		// This splits the current line at the insertion point

		// First line gets content before col + first line of new text
		firstLineRunes := []rune(stringLines[0])
		firstLine := make([]rune, 0, col+len(firstLineRunes))
		firstLine = append(firstLine, line[:col]...)
		firstLine = append(firstLine, firstLineRunes...)

		// Last line gets last line of new text + content after col
		lastLineRunes := []rune(stringLines[len(stringLines)-1])
		lastLine := make([]rune, 0, len(lastLineRunes)+len(line)-col)
		lastLine = append(lastLine, lastLineRunes...)
		lastLine = append(lastLine, line[col:]...)

		// Replace current line with first line of result
		b.lines[row] = firstLine

		// Insert all middle lines (if any)
		insertPos := row + 1
		for i := 1; i < len(stringLines)-1; i++ {
			b.insertLine(insertPos, stringLines[i])
			insertPos++
		}

		// Insert the last line
		b.lines = slices.Insert(b.lines, insertPos, lastLine)
	}
}

// deleteAt deletes text between the specified positions
// by converting to cursor positions and using deleteRange
func (b *buffer) deleteAt(startRow, startCol, endRow, endCol int) string {
	start := Cursor{Row: startRow, Col: startCol}
	end := Cursor{Row: endRow, Col: endCol}
	return b.deleteRange(start, end)
}

// areSlicesEqual checks if two [][]rune slices have identical content
func areSlicesEqual(a, b [][]rune) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if len(a[i]) != len(b[i]) {
			return false
		}
		for j := range a[i] {
			if a[i][j] != b[i][j] {
				return false
			}
		}
	}
	return true
}

// saveUndoState saves the current buffer state to the undo stack
// This should be called before making changes to the buffer
// If the content is identical to the previous state, no new state is saved
func (b *buffer) saveUndoState(cursor Cursor) {
	// Check if there's a previous state with identical content
	if len(b.undoStack) > 0 {
		lastState := b.undoStack[len(b.undoStack)-1]
		if areSlicesEqual(b.lines, lastState.lines) {
			// Content is identical, no need to save a new state
			return
		}
	}

	// Create a deep copy of the current lines
	contentCopy := make([][]rune, len(b.lines))
	for i, line := range b.lines {
		lineCopy := make([]rune, len(line))
		copy(lineCopy, line)
		contentCopy[i] = lineCopy
	}

	// Add the current state to the undo stack
	b.undoStack = append(b.undoStack, bufferState{lines: contentCopy, cursor: cursor})

	// Clear the redo stack since we've made a new change
	b.redoStack = []bufferState{}

	// Limit the undo stack size to prevent memory issues
	const maxUndoSize = 100
	if len(b.undoStack) > maxUndoSize {
		b.undoStack = b.undoStack[len(b.undoStack)-maxUndoSize:]
	}
}

// undo reverts to the previous buffer state
// Returns a command that updates the cursor position
func (b *buffer) undo(c Cursor) tea.Cmd {
	return func() tea.Msg {
		if len(b.undoStack) == 0 {
			return UndoRedoMsg{Success: false, IsUndo: true}
		}

		// Get the last state from undo stack
		lastIdx := len(b.undoStack) - 1
		lastState := b.undoStack[lastIdx]

		// Remove it from the undo stack
		b.undoStack = b.undoStack[:lastIdx]

		// Save current state to redo stack
		contentCopy := make([][]rune, len(b.lines))
		for i, line := range b.lines {
			lineCopy := make([]rune, len(line))
			copy(lineCopy, line)
			contentCopy[i] = lineCopy
		}

		b.redoStack = append(b.redoStack, bufferState{
			lines:  contentCopy,
			cursor: c,
		})

		// Restore the previous state
		b.lines = lastState.lines

		// Return a message with the new cursor position
		return UndoRedoMsg{
			NewCursor: lastState.cursor,
			Success:   true,
			IsUndo:    true,
		}
	}
}

// redo reapplies a previously undone change
// Returns a command that updates the cursor position
func (b *buffer) redo(c Cursor) tea.Cmd {
	return func() tea.Msg {
		if len(b.redoStack) == 0 {
			return UndoRedoMsg{Success: false, IsUndo: false}
		}

		// Get the last state from redo stack
		lastIdx := len(b.redoStack) - 1
		lastState := b.redoStack[lastIdx]

		// Remove it from the redo stack
		b.redoStack = b.redoStack[:lastIdx]

		// Save current state to undo stack
		contentCopy := make([][]rune, len(b.lines))
		for i, line := range b.lines {
			lineCopy := make([]rune, len(line))
			copy(lineCopy, line)
			contentCopy[i] = lineCopy
		}

		b.undoStack = append(b.undoStack, bufferState{
			lines:  contentCopy,
			cursor: c,
		})

		// Restore the state from redo stack
		b.lines = lastState.lines

		// Return a message with the new cursor position
		return UndoRedoMsg{
			NewCursor: lastState.cursor,
			Success:   true,
			IsUndo:    false,
		}
	}
}

// canUndo returns whether there are changes that can be undone
func (b *buffer) canUndo() bool {
	return len(b.undoStack) > 0
}

// canRedo returns whether there are changes that can be redone
func (b *buffer) canRedo() bool {
	return len(b.redoStack) > 0
}

// getRange returns the text between two cursor positions
// Ensures that start is before end
func (b *buffer) getRange(start, end Cursor) string {
	// Ensure start is before end
	if start.Row > end.Row || (start.Row == end.Row && start.Col > end.Col) {
		start, end = end, start
	}

	// Handle single line case
	if start.Row == end.Row {
		line := b.lineRunes(start.Row)
		endCol := min(end.Col+1, len(line))
		return string(line[start.Col:endCol])
	}

	// Handle multi-line case
	var result strings.Builder

	// First line (from start column to end of line)
	firstLine := b.lineRunes(start.Row)
	result.WriteString(string(firstLine[start.Col:]))
	result.WriteString("\n")

	// Middle lines (full lines)
	for i := start.Row + 1; i < end.Row; i++ {
		result.WriteString(b.Line(i))
		result.WriteString("\n")
	}

	// Last line (from beginning to end column)
	lastLine := b.lineRunes(end.Row)
	endCol := min(end.Col+1, len(lastLine))
	result.WriteString(string(lastLine[:endCol]))

	return result.String()
}

// joinLines concatenates two lines, removing the line break between them
func (b *buffer) joinLines(row, nextRow int) {
	if row < 0 || nextRow >= len(b.lines) || row >= nextRow {
		return
	}

	firstLine := b.Line(row)
	secondLine := b.Line(nextRow)

	b.setLine(row, firstLine+secondLine)
	b.deleteLine(nextRow)
}

// deleteRange removes the text between start and end positions and returns the deleted text
// This is the core function for text deletion operations
func (b *buffer) deleteRange(start, end Cursor) string {
	// Ensure start is before end
	if start.Row > end.Row || (start.Row == end.Row && start.Col > end.Col) {
		start, end = end, start
	}

	// Get the text that will be deleted
	deletedText := b.getRange(start, end)

	// Handle single line case
	if start.Row == end.Row {
		line := b.lineRunes(start.Row)
		endCol := min(end.Col+1, len(line))
		newLine := make([]rune, 0, len(line)-(endCol-start.Col))
		newLine = append(newLine, line[:start.Col]...)
		newLine = append(newLine, line[endCol:]...)
		b.lines[start.Row] = newLine
		return deletedText
	}

	// Special case for joining lines (when selection ends at start of next line)
	if start.Col == b.lineLength(start.Row) && end.Col == 0 && end.Row == start.Row+1 {
		b.joinLines(start.Row, end.Row)
		return deletedText
	}

	// Handle multi-line case
	firstLine := b.lineRunes(start.Row)
	lastLine := b.lineRunes(end.Row)
	endCol := min(end.Col+1, len(lastLine))

	// Join the start of first line with the end of last line
	newLine := make([]rune, 0, start.Col+len(lastLine)-endCol)
	newLine = append(newLine, firstLine[:start.Col]...)
	newLine = append(newLine, lastLine[endCol:]...)
	b.lines[start.Row] = newLine

	// Remove all lines in between
	for range end.Row - start.Row {
		b.deleteLine(start.Row + 1)
	}

	return deletedText
}
