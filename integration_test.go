package vimtea

import (
	"regexp"
	"testing"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// stripAnsi removes ANSI escape codes from a string
func stripAnsi(s string) string {
	ansiRegex := regexp.MustCompile(`\x1b\[[0-9;]*[a-zA-Z]`)
	return ansiRegex.ReplaceAllString(s, "")
}

// validateUTF8AroundAnsi checks that ANSI codes don't split UTF-8 characters
func validateUTF8AroundAnsi(t *testing.T, s string) {
	ansiRegex := regexp.MustCompile(`\x1b\[[0-9;]*[a-zA-Z]`)
	matches := ansiRegex.FindAllStringIndex(s, -1)

	for _, match := range matches {
		start := match[0]
		end := match[1]

		// Check byte before ANSI code (if exists)
		if start > 0 {
			// Get up to 4 bytes before to check for valid UTF-8 character boundary
			checkStart := start
			if checkStart > 4 {
				checkStart = start - 4
			} else {
				checkStart = 0
			}
			beforeAnsi := s[checkStart:start]

			// The string before ANSI must be valid UTF-8
			assert.True(t, isValidUTF8(beforeAnsi),
				"Text before ANSI code must be valid UTF-8, got bytes: %v", []byte(beforeAnsi))

			// Check if we're at a character boundary (not in the middle of a multi-byte char)
			if start > 0 {
				// The byte right before ANSI should not be a continuation byte (10xxxxxx)
				prevByte := s[start-1]
				assert.False(t, (prevByte&0xC0) == 0x80,
					"ANSI code inserted in the middle of UTF-8 character! Byte before ANSI: 0x%02x", prevByte)
			}
		}

		// Check byte after ANSI code (if exists)
		if end < len(s) {
			checkEnd := end + 4
			if checkEnd > len(s) {
				checkEnd = len(s)
			}
			afterAnsi := s[end:checkEnd]

			// The string after ANSI must be valid UTF-8
			assert.True(t, isValidUTF8(afterAnsi),
				"Text after ANSI code must be valid UTF-8, got bytes: %v", []byte(afterAnsi))
		}
	}
}

// isValidUTF8 checks if a string contains only valid UTF-8
func isValidUTF8(s string) bool {
	for len(s) > 0 {
		r, size := rune(s[0]), 1
		if s[0] >= 0x80 {
			var ok bool
			r, size = decodeRune(s)
			if r == '\uFFFD' { // replacement character indicates invalid UTF-8
				return false
			}
			if !ok {
				return false
			}
		}
		s = s[size:]
	}
	return true
}

// decodeRune decodes the first rune in s
func decodeRune(s string) (rune, int) {
	if len(s) == 0 {
		return '\uFFFD', 0
	}

	b0 := s[0]

	// 1-byte (ASCII)
	if b0 < 0x80 {
		return rune(b0), 1
	}

	// 2-byte
	if b0&0xE0 == 0xC0 {
		if len(s) < 2 {
			return '\uFFFD', 1
		}
		b1 := s[1]
		if b1&0xC0 != 0x80 {
			return '\uFFFD', 1
		}
		r := rune(b0&0x1F)<<6 | rune(b1&0x3F)
		if r < 0x80 {
			return '\uFFFD', 1
		}
		return r, 2
	}

	// 3-byte
	if b0&0xF0 == 0xE0 {
		if len(s) < 3 {
			return '\uFFFD', 1
		}
		b1, b2 := s[1], s[2]
		if b1&0xC0 != 0x80 || b2&0xC0 != 0x80 {
			return '\uFFFD', 1
		}
		r := rune(b0&0x0F)<<12 | rune(b1&0x3F)<<6 | rune(b2&0x3F)
		if r < 0x800 {
			return '\uFFFD', 1
		}
		return r, 3
	}

	// 4-byte
	if b0&0xF8 == 0xF0 {
		if len(s) < 4 {
			return '\uFFFD', 1
		}
		b1, b2, b3 := s[1], s[2], s[3]
		if b1&0xC0 != 0x80 || b2&0xC0 != 0x80 || b3&0xC0 != 0x80 {
			return '\uFFFD', 1
		}
		r := rune(b0&0x07)<<18 | rune(b1&0x3F)<<12 | rune(b2&0x3F)<<6 | rune(b3&0x3F)
		if r < 0x10000 || r > 0x10FFFF {
			return '\uFFFD', 1
		}
		return r, 4
	}

	return '\uFFFD', 1
}

func TestEditorIntegration(t *testing.T) {
	initialContent := "Hello, world!"
	editor := NewEditor(WithContent(initialContent))

	assert.Equal(t, ModeNormal, editor.GetMode(), "Initial mode should be Normal")

	buffer := editor.GetBuffer()
	assert.Equal(t, initialContent, buffer.Text(), "Buffer content should match initial content")

	editor.SetMode(ModeInsert)
	assert.Equal(t, ModeInsert, editor.GetMode(), "Mode should be Insert after setting")

	model := editor.(*editorModel)
	model.buffer.insertAt(0, 13, " This is a test.")

	expectedContent := "Hello, world! This is a test."
	assert.Equal(t, expectedContent, buffer.Text(), "Buffer content should match expected after insertion")

	editor.SetMode(ModeNormal)
	assert.Equal(t, ModeNormal, editor.GetMode(), "Mode should be Normal after setting")

	testStatusMsg := "Test status"
	cmd := editor.SetStatusMessage(testStatusMsg)
	cmd()

	assert.Equal(t, testStatusMsg, model.statusMessage, "Status message should match set message")
}

func TestViewportIntegration(t *testing.T) {
	var content string
	for i := 0; i < 30; i++ {
		content += "Line " + string(rune('A'+i%26)) + "\n"
	}

	editor := NewEditor(WithContent(content))
	model := editor.(*editorModel)

	model.width = 80
	model.height = 20
	model.viewport.Width = 80
	model.viewport.Height = 20

	model.cursor = newCursor(25, 0)

	model.ensureCursorVisible()

	assert.GreaterOrEqual(t, model.cursor.Row, model.viewport.YOffset,
		"Cursor row should be within or after viewport start")
	assert.Less(t, model.cursor.Row, model.viewport.YOffset+model.viewport.Height,
		"Cursor row should be within viewport end")
}

func TestKeyBindingsIntegration(t *testing.T) {
	editor := NewEditor()
	model := editor.(*editorModel)

	testBindingCalled := false
	editor.AddBinding(KeyBinding{
		Key:         "ctrl+t",
		Mode:        ModeNormal,
		Description: "Test binding",
		Handler: func(b Buffer) tea.Cmd {
			testBindingCalled = true
			return nil
		},
	})

	iBinding := model.registry.FindExact("i", ModeNormal)
	assert.NotNil(t, iBinding, "Default binding for 'i' should exist")

	ctrlTBinding := model.registry.FindExact("ctrl+t", ModeNormal)
	require.NotNil(t, ctrlTBinding, "Custom binding for 'ctrl+t' should exist")

	_ = ctrlTBinding.Command(model)

	assert.True(t, testBindingCalled, "Custom binding command should have been executed")
}

func TestCommandsIntegration(t *testing.T) {
	editor := NewEditor()
	model := editor.(*editorModel)

	commandCalled := false
	editor.AddCommand("test", func(b Buffer, args []string) tea.Cmd {
		commandCalled = true
		if len(args) > 0 && args[0] == "arg" {
			return nil
		}
		return nil
	})

	model.commandBuffer = "test arg"
	model.Update(CommandMsg{Command: "test"})

	assert.True(t, commandCalled, "Command should have been executed")
}

func TestClearIntegration(t *testing.T) {
	// Create editor with initial content
	initialContent := "Line 1\nLine 2\nLine 3"
	editor := NewEditor(WithContent(initialContent))
	buffer := editor.GetBuffer()

	// Verify initial content
	assert.Equal(t, initialContent, buffer.Text(), "Buffer should have initial content")
	assert.Equal(t, 3, buffer.LineCount(), "Buffer should have 3 lines initially")

	// Set cursor to a non-zero position
	model := editor.(*editorModel)
	model.cursor = newCursor(1, 3)

	// Clear the buffer using the Clear method
	clearCmd := buffer.Clear()
	if clearCmd != nil {
		clearCmd()
	}

	// After clearing, the buffer should have a single empty line and cursor at 0,0
	assert.Equal(t, 1, buffer.LineCount(), "Buffer should have 1 line after clear")
	assert.Equal(t, "", buffer.Text(), "Buffer text should be empty")
	assert.Equal(t, 0, model.cursor.Row, "Cursor row should be reset to 0")
	assert.Equal(t, 0, model.cursor.Col, "Cursor column should be reset to 0")

	// Test undo functionality after clear
	undoCmd := buffer.Undo()
	undoResult := undoCmd().(UndoRedoMsg)

	assert.True(t, undoResult.Success, "Undo after clear should succeed")
	assert.Equal(t, initialContent, buffer.Text(), "Buffer should return to initial content after undo")
}

func TestResetIntegration(t *testing.T) {
	// Create editor with initial content
	initialContent := "Initial content"
	editor := NewEditor(WithContent(initialContent))
	buffer := editor.GetBuffer()

	// Verify initial content
	assert.Equal(t, initialContent, buffer.Text(), "Buffer should have initial content")

	// Make changes to the editor
	model := editor.(*editorModel)
	buffer.InsertAt(0, 0, "Modified ") // Modify the content
	model.cursor = newCursor(0, 9)     // Move cursor after "Modified "

	// Verify the changes were made
	assert.Equal(t, "Modified Initial content", buffer.Text(), "Buffer content should be modified")
	assert.Equal(t, 0, model.cursor.Row, "Cursor row should be 0")
	assert.Equal(t, 9, model.cursor.Col, "Cursor column should be 9")

	// Reset the editor
	resetCmd := editor.Reset()
	if resetCmd != nil {
		resetCmd()
	}

	// Verify the editor has been reset to initial state
	assert.Equal(t, initialContent, buffer.Text(), "Buffer should be reset to initial content")
	assert.Equal(t, 0, model.cursor.Row, "Cursor row should be reset to 0")
	assert.Equal(t, 0, model.cursor.Col, "Cursor column should be reset to 0")
	assert.Equal(t, ModeNormal, model.mode, "Editor mode should be reset to Normal")
	assert.Equal(t, "", model.yankBuffer, "Yank buffer should be empty")

	// Make more changes after reset
	buffer.InsertAt(0, 0, "New ")
	assert.Equal(t, "New Initial content", buffer.Text(), "Buffer should accept changes after reset")

	// Reset again
	resetCmd = editor.Reset()
	if resetCmd != nil {
		resetCmd()
	}

	// Verify reset again
	assert.Equal(t, initialContent, buffer.Text(), "Buffer should be reset to initial content again")
}

func TestUTF8CharacterInput(t *testing.T) {
	// Test that UTF-8 characters (including Polish diacritical marks) can be inserted
	editor := NewEditor(WithContent(""))
	model := editor.(*editorModel)
	buffer := editor.GetBuffer()

	// Switch to insert mode
	editor.SetMode(ModeInsert)
	assert.Equal(t, ModeInsert, model.mode, "Should be in insert mode")

	// Test inserting Polish diacritical characters
	polishChars := []string{"ą", "ć", "ę", "ł", "ń", "ó", "ś", "ź", "ż"}

	for _, char := range polishChars {
		// Create a key message with the Polish character
		keyMsg := tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune(char)}

		// Process the key message
		_, _ = model.handleKeypress(keyMsg)
	}

	// Verify all Polish characters were inserted
	expectedText := "ąćęłńóśźż"
	assert.Equal(t, expectedText, buffer.Text(), "All Polish diacritical characters should be inserted")

	// Test in command mode
	editor.SetMode(ModeCommand)
	assert.Equal(t, ModeCommand, model.mode, "Should be in command mode")

	// Insert Polish characters in command buffer
	for _, char := range []string{"ć", "ó", "ś"} {
		keyMsg := tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune(char)}
		_, _ = model.handleKeypress(keyMsg)
	}

	// Verify command buffer contains Polish characters
	assert.Equal(t, "ćóś", model.commandBuffer, "Command buffer should contain Polish characters")
}

func TestUTF8BackspaceInInsertMode(t *testing.T) {
	// Test that backspace correctly deletes entire UTF-8 characters
	editor := NewEditor(WithContent(""))
	model := editor.(*editorModel)
	buffer := editor.GetBuffer()

	// Switch to insert mode
	editor.SetMode(ModeInsert)

	// Insert Polish characters
	polishChars := []string{"ą", "ć", "ę"}
	for _, char := range polishChars {
		keyMsg := tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune(char)}
		_, _ = model.handleKeypress(keyMsg)
	}

	// Verify all characters were inserted
	assert.Equal(t, "ąćę", buffer.Text(), "All Polish characters should be inserted")

	// Delete one character with backspace
	backspaceMsg := tea.KeyMsg{Type: tea.KeyBackspace}
	_, _ = model.handleKeypress(backspaceMsg)

	// Should have deleted entire "ę" character, not just one byte
	assert.Equal(t, "ąć", buffer.Text(), "Backspace should delete entire UTF-8 character")

	// Delete another character
	_, _ = model.handleKeypress(backspaceMsg)
	assert.Equal(t, "ą", buffer.Text(), "Backspace should delete second UTF-8 character")

	// Delete last character
	_, _ = model.handleKeypress(backspaceMsg)
	assert.Equal(t, "", buffer.Text(), "Backspace should delete last UTF-8 character")
}

func TestUTF8BackspaceInCommandMode(t *testing.T) {
	// Test that backspace correctly deletes entire UTF-8 characters in command mode
	editor := NewEditor(WithContent(""))
	model := editor.(*editorModel)

	// Switch to command mode
	editor.SetMode(ModeCommand)

	// Insert Polish characters in command buffer
	polishChars := []string{"ł", "ń", "ó"}
	for _, char := range polishChars {
		keyMsg := tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune(char)}
		_, _ = model.handleKeypress(keyMsg)
	}

	// Verify all characters were inserted
	assert.Equal(t, "łńó", model.commandBuffer, "All Polish characters should be in command buffer")

	// Delete one character with backspace
	backspaceMsg := tea.KeyMsg{Type: tea.KeyBackspace}
	_, _ = model.handleKeypress(backspaceMsg)

	// Should have deleted entire "ó" character
	assert.Equal(t, "łń", model.commandBuffer, "Backspace should delete entire UTF-8 character in command mode")

	// Delete another character
	_, _ = model.handleKeypress(backspaceMsg)
	assert.Equal(t, "ł", model.commandBuffer, "Backspace should delete second UTF-8 character")

	// Delete last character
	_, _ = model.handleKeypress(backspaceMsg)
	assert.Equal(t, "", model.commandBuffer, "Backspace should delete last UTF-8 character")
}

func TestUTF8CursorMovement(t *testing.T) {
	// Test that cursor moves by characters, not bytes
	content := "ąćę"
	editor := NewEditor(WithContent(content))
	model := editor.(*editorModel)

	// Start in normal mode at position 0,0
	assert.Equal(t, ModeNormal, model.mode, "Should be in normal mode")
	assert.Equal(t, 0, model.cursor.Col, "Cursor should start at column 0")

	// Move right once - should move to 'ć' (character position 1)
	rightMsg := tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune("l")}
	_, _ = model.handleKeypress(rightMsg)
	assert.Equal(t, 1, model.cursor.Col, "Cursor should be at character position 1 ('ć')")

	// Move right again - should move to 'ę' (character position 2)
	_, _ = model.handleKeypress(rightMsg)
	assert.Equal(t, 2, model.cursor.Col, "Cursor should be at character position 2 ('ę')")

	// Move left once - should move back to 'ć' (character position 1)
	leftMsg := tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune("h")}
	_, _ = model.handleKeypress(leftMsg)
	assert.Equal(t, 1, model.cursor.Col, "Cursor should be at character position 1 ('ć')")

	// Move left again - should move back to 'ą' (character position 0)
	_, _ = model.handleKeypress(leftMsg)
	assert.Equal(t, 0, model.cursor.Col, "Cursor should be at character position 0 ('ą')")

	// Test with mixed ASCII and UTF-8
	content2 := "abc-ąćę-xyz"
	editor2 := NewEditor(WithContent(content2))
	model2 := editor2.(*editorModel)

	// Move to position 4 (the 'ą')
	model2.cursor.Col = 4

	// Move right - should move to 'ć' (character position 5)
	_, _ = model2.handleKeypress(rightMsg)
	assert.Equal(t, 5, model2.cursor.Col, "Cursor should be at character position 5 ('ć')")

	// Move left - should move back to 'ą' (character position 4)
	_, _ = model2.handleKeypress(leftMsg)
	assert.Equal(t, 4, model2.cursor.Col, "Cursor should be at character position 4 ('ą')")

	// Move left - should move to '-' (character position 3)
	_, _ = model2.handleKeypress(leftMsg)
	assert.Equal(t, 3, model2.cursor.Col, "Cursor should be at character position 3 ('-')")
}

func TestUTF8VerticalMovement(t *testing.T) {
	// Test that vertical cursor movement maintains valid UTF-8 positions
	content := "łódź\nżółć\nąćę"
	editor := NewEditor(WithContent(content))
	model := editor.(*editorModel)

	// Start at row 0, col 0 (on 'ł')
	assert.Equal(t, 0, model.cursor.Row, "Should start at row 0")
	assert.Equal(t, 0, model.cursor.Col, "Should start at col 0")

	// Move right twice to 'ó' (character position 2)
	rightMsg := tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune("l")}
	_, _ = model.handleKeypress(rightMsg)
	_, _ = model.handleKeypress(rightMsg)
	assert.Equal(t, 2, model.cursor.Col, "Cursor should be at character 2 (on 'ó')")

	// Move down to second line - should try to maintain column 2
	downMsg := tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune("j")}
	_, _ = model.handleKeypress(downMsg)
	assert.Equal(t, 1, model.cursor.Row, "Should be on row 1")
	// Line "żółć": ż(0) ó(1) ł(2) ć(3)
	// desiredCol=2, should land on 'ł' at position 2
	assert.Equal(t, 2, model.cursor.Col, "Cursor should be at character position 2")

	// Move down to third line (shorter line)
	_, _ = model.handleKeypress(downMsg)
	assert.Equal(t, 2, model.cursor.Row, "Should be on row 2")
	// Line "ąćę": ą(0) ć(1) ę(2)
	// desiredCol=2, should land on 'ę' at position 2
	assert.Equal(t, 2, model.cursor.Col, "Cursor should be at character position 2")

	// Move to end of line
	endMsg := tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune("$")}
	_, _ = model.handleKeypress(endMsg)
	assert.Equal(t, 2, model.cursor.Col, "Cursor should be at last character 'ę'")

	// Move up - should maintain column or adjust to valid position
	upMsg := tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune("k")}
	_, _ = model.handleKeypress(upMsg)
	assert.Equal(t, 1, model.cursor.Row, "Should be on row 1")
	// Should be at a valid character position
	lineLen := model.buffer.lineLength(1)
	assert.GreaterOrEqual(t, lineLen, model.cursor.Col, "Cursor should be within line bounds")
}

func TestUTF8CursorRendering(t *testing.T) {
	// Test that cursor renders correctly in the middle of Polish words
	content := "żółć"
	editor := NewEditor(WithContent(content))
	model := editor.(*editorModel)

	// Set up minimal dimensions for rendering
	model.width = 80
	model.height = 20
	model.viewport.Width = 80
	model.viewport.Height = 20

	// Test cursor at beginning of word (on 'ż')
	model.cursor = newCursor(0, 0)
	model.cursorBlink = true
	view := model.View()

	// CRITICAL: Validate that ANSI codes don't corrupt UTF-8 byte sequences
	validateUTF8AroundAnsi(t, view)

	// Strip ANSI codes to check the actual text content
	cleanView := stripAnsi(view)

	// View should not be empty
	assert.NotEmpty(t, view, "View should not be empty")

	// The clean view should contain the complete word - this verifies no bytes are corrupted
	assert.Contains(t, cleanView, "żółć", "Cleaned view should contain complete word 'żółć'")

	// Also verify individual characters are not corrupted
	assert.Contains(t, cleanView, "ż", "View should contain 'ż'")
	assert.Contains(t, cleanView, "ó", "View should contain 'ó'")
	assert.Contains(t, cleanView, "ł", "View should contain 'ł'")
	assert.Contains(t, cleanView, "ć", "View should contain 'ć'")

	// Test cursor in middle of word (on 'ó' - character position 1)
	model.cursor = newCursor(0, 1)
	view = model.View()
	validateUTF8AroundAnsi(t, view) // Validate byte integrity
	cleanView = stripAnsi(view)

	assert.NotEmpty(t, view, "View should not be empty with cursor at position 1")
	assert.Contains(t, cleanView, "żółć", "Cleaned view should contain complete word with cursor on 'ó'")
	assert.NotContains(t, cleanView, "Å�", "View should not contain garbled UTF-8 artifacts")

	// Test cursor on third character (on 'ł' - character position 2)
	model.cursor = newCursor(0, 2)
	view = model.View()
	validateUTF8AroundAnsi(t, view) // Validate byte integrity
	cleanView = stripAnsi(view)

	assert.NotEmpty(t, view, "View should not be empty with cursor at position 2")
	assert.Contains(t, cleanView, "żółć", "Cleaned view should contain complete word with cursor on 'ł'")
	assert.NotContains(t, cleanView, "Å�", "View should not contain garbled UTF-8 artifacts")

	// Test cursor on last character (on 'ć' - character position 3)
	model.cursor = newCursor(0, 3)
	view = model.View()
	validateUTF8AroundAnsi(t, view) // Validate byte integrity
	cleanView = stripAnsi(view)

	assert.NotEmpty(t, view, "View should not be empty with cursor at position 3")
	assert.Contains(t, cleanView, "żółć", "Cleaned view should contain complete word with cursor on 'ć'")
	assert.NotContains(t, cleanView, "Å�", "View should not contain garbled UTF-8 artifacts")

	// Test with a longer text containing Polish characters
	content2 := "Witaj świecie! Cześć żółć ąćęłńóśźż"
	editor2 := NewEditor(WithContent(content2))
	model2 := editor2.(*editorModel)
	model2.width = 80
	model2.height = 20
	model2.viewport.Width = 80
	model2.viewport.Height = 20

	// Move cursor to the 'ó' in "żółć" (position 19 in the string)
	model2.cursor = newCursor(0, 19)
	model2.cursorBlink = true
	view2 := model2.View()

	// CRITICAL: Validate UTF-8 integrity around ALL ANSI codes
	validateUTF8AroundAnsi(t, view2)

	cleanView2 := stripAnsi(view2)

	assert.NotEmpty(t, view2, "View should not be empty")
	assert.NotContains(t, cleanView2, "Å�", "View should not contain garbled UTF-8 artifacts")

	// Verify the complete content is preserved without corruption
	assert.Contains(t, cleanView2, "Witaj świecie!", "View should contain 'Witaj świecie!'")
	assert.Contains(t, cleanView2, "Cześć", "View should contain 'Cześć'")
	assert.Contains(t, cleanView2, "żółć", "View should contain complete word 'żółć'")
	assert.Contains(t, cleanView2, "ąćęłńóśźż", "View should contain all Polish diacritical marks")
}

func TestUTF8EnterKeySplit(t *testing.T) {
	// Test for the bug: pressing Enter at end of line "żółć" incorrectly splits the line
	editor := NewEditor(WithContent("żółć"))
	model := editor.(*editorModel)

	// Position cursor at end of line "żółć" (position 4)
	model.cursor = newCursor(0, 4)
	assert.Equal(t, 4, model.cursor.Col, "Cursor should be at position 4 (end of line)")

	// Switch to insert mode
	model.mode = ModeInsert

	// Simulate pressing Enter key
	handleInsertEnterKey(model)

	// After pressing Enter at the end of "żółć", we should have:
	// Line 0: "żółć"
	// Line 1: "" (empty)
	assert.Equal(t, 2, model.buffer.lineCount(), "Buffer should have 2 lines after Enter")
	assert.Equal(t, "żółć", model.buffer.Line(0), "First line should remain 'żółć'")
	assert.Equal(t, "", model.buffer.Line(1), "Second line should be empty")
	assert.Equal(t, 1, model.cursor.Row, "Cursor should move to row 1")
	assert.Equal(t, 0, model.cursor.Col, "Cursor should be at column 0")
}

func TestUTF8EnterKeySplitMiddle(t *testing.T) {
	// Test pressing Enter in the middle of Polish word
	editor := NewEditor(WithContent("żółć"))
	model := editor.(*editorModel)

	// Position cursor at position 2 (after "żó", before "łć")
	model.cursor = newCursor(0, 2)
	model.mode = ModeInsert

	// Simulate pressing Enter key
	handleInsertEnterKey(model)

	// After pressing Enter at position 2, we should have:
	// Line 0: "żó"
	// Line 1: "łć"
	assert.Equal(t, 2, model.buffer.lineCount(), "Buffer should have 2 lines after Enter")
	assert.Equal(t, "żó", model.buffer.Line(0), "First line should be 'żó'")
	assert.Equal(t, "łć", model.buffer.Line(1), "Second line should be 'łć'")
	assert.Equal(t, 1, model.cursor.Row, "Cursor should move to row 1")
	assert.Equal(t, 0, model.cursor.Col, "Cursor should be at column 0")
}

func TestUTF8CursorMovementThroughWord(t *testing.T) {
	// Test moving cursor through "żółć" character by character
	editor := NewEditor(WithContent("żółć"))
	model := editor.(*editorModel)

	// Set up minimal dimensions for rendering
	model.width = 80
	model.height = 20
	model.viewport.Width = 80
	model.viewport.Height = 20

	// Start at position 0 (on 'ż')
	model.cursor = newCursor(0, 0)

	// Test each position
	positions := []struct {
		col      int
		expected string // The character the cursor should be on
	}{
		{0, "ż"},
		{1, "ó"},
		{2, "ł"},
		{3, "ć"},
	}

	for _, pos := range positions {
		model.cursor.Col = pos.col

		// Get the line as runes
		line := model.buffer.Line(0)
		runes := []rune(line)

		// Verify cursor position is valid
		assert.True(t, pos.col < len(runes), "Position %d should be within line length %d", pos.col, len(runes))

		// Get character at cursor position
		if pos.col < len(runes) {
			char := string(runes[pos.col])
			assert.Equal(t, pos.expected, char, "At position %d, cursor should be on '%s'", pos.col, pos.expected)
		}

		// Render and verify no corruption
		view := model.View()
		cleanView := stripAnsi(view)
		assert.Contains(t, cleanView, "żółć", "View at position %d should contain complete word 'żółć'", pos.col)

		// Verify UTF-8 integrity around ANSI codes
		validateUTF8AroundAnsi(t, view)
	}
}

func TestUTF8CursorNavigationWithLH(t *testing.T) {
	// Test using l and h commands to navigate through "żółć"
	editor := NewEditor(WithContent("żółć"))
	model := editor.(*editorModel)
	model.mode = ModeNormal

	// Set up minimal dimensions for rendering
	model.width = 80
	model.height = 20
	model.viewport.Width = 80
	model.viewport.Height = 20

	// Start at position 0
	model.cursor = newCursor(0, 0)
	assert.Equal(t, 0, model.cursor.Col, "Should start at position 0")

	// Press 'l' to move right through each character
	lBinding := model.registry.FindExact("l", ModeNormal)
	require.NotNil(t, lBinding, "Binding for 'l' not found")

	// Move to 'ó' (position 1)
	lBinding.Command(model)
	assert.Equal(t, 1, model.cursor.Col, "After 'l', should be at position 1 (ó)")

	// Move to 'ł' (position 2)
	lBinding.Command(model)
	assert.Equal(t, 2, model.cursor.Col, "After 2x 'l', should be at position 2 (ł)")

	// Move to 'ć' (position 3)
	lBinding.Command(model)
	assert.Equal(t, 3, model.cursor.Col, "After 3x 'l', should be at position 3 (ć)")

	// Now move back with 'h'
	hBinding := model.registry.FindExact("h", ModeNormal)
	require.NotNil(t, hBinding, "Binding for 'h' not found")

	// Move back to 'ł' (position 2)
	hBinding.Command(model)
	assert.Equal(t, 2, model.cursor.Col, "After 'h', should be at position 2 (ł)")

	// Move back to 'ó' (position 1)
	hBinding.Command(model)
	assert.Equal(t, 1, model.cursor.Col, "After 2x 'h', should be at position 1 (ó)")

	// Move back to 'ż' (position 0)
	hBinding.Command(model)
	assert.Equal(t, 0, model.cursor.Col, "After 3x 'h', should be at position 0 (ż)")

	// Verify the view at final position
	view := model.View()
	cleanView := stripAnsi(view)
	assert.Contains(t, cleanView, "żółć", "Final view should contain complete word 'żółć'")
	validateUTF8AroundAnsi(t, view)
}

func TestUTF8CursorWithSyntaxHighlighting(t *testing.T) {
	// Test the specific bug: cursor rendering with syntax highlighting on UTF-8 text
	// This was causing "�óółć" to appear instead of "żółć" when moving cursor
	content := "żółć"
	editor := NewEditor(
		WithContent(content),
		WithFileName("test.go"), // .go files have syntax highlighting
	)
	model := editor.(*editorModel)

	// Set up dimensions for rendering
	model.width = 80
	model.height = 20
	model.viewport.Width = 80
	model.viewport.Height = 20
	model.mode = ModeNormal

	// Test cursor at each position with syntax highlighting enabled
	positions := []struct {
		col          int
		charAtCursor string
	}{
		{0, "ż"},
		{1, "ó"},
		{2, "ł"},
		{3, "ć"},
	}

	for _, pos := range positions {
		model.cursor.Col = pos.col

		// Render the view
		view := model.View()

		// Check that there are no UTF-8 replacement characters (�)
		// This is the main bug we're testing for
		assert.NotContains(t, view, "�", "View should not contain UTF-8 replacement character at position %d", pos.col)

		// Strip ANSI and verify complete word is present
		cleanView := stripAnsi(view)
		assert.Contains(t, cleanView, "żółć", "View at position %d should contain complete word 'żółć'", pos.col)

		// Note: We don't validate ANSI code placement here because the syntax
		// highlighting library may insert codes at byte boundaries. The important
		// thing is that our code doesn't corrupt UTF-8 characters (no � appears)

		// Verify the character at cursor position in buffer is correct
		line := model.buffer.Line(0)
		runes := []rune(line)
		if pos.col < len(runes) {
			char := string(runes[pos.col])
			assert.Equal(t, pos.charAtCursor, char, "Character at buffer position %d should be '%s'", pos.col, pos.charAtCursor)
		}
	}
}

func TestUTF8CursorMovementWithSyntaxHighlighting(t *testing.T) {
	// Test moving cursor through Polish text with syntax highlighting
	// This reproduces the original bug report where moving cursor left showed "�óółć"
	content := "żółć"
	editor := NewEditor(
		WithContent(content),
		WithFileName("test.txt"), // Even .txt might have basic highlighting
	)
	model := editor.(*editorModel)

	// Set up dimensions for rendering
	model.width = 80
	model.height = 20
	model.viewport.Width = 80
	model.viewport.Height = 20
	model.mode = ModeNormal

	// Start at position 0
	model.cursor = newCursor(0, 0)

	// Move cursor through all positions
	lBinding := model.registry.FindExact("l", ModeNormal)
	require.NotNil(t, lBinding, "Binding for 'l' not found")

	// Test at initial position (position 0, on 'ż')
	view := model.View()
	assert.NotContains(t, view, "�", "View should not contain replacement character at position 0")
	cleanView := stripAnsi(view)
	assert.Contains(t, cleanView, "żółć", "View should contain 'żółć' at position 0")

	// Move to position 1 (on 'ó')
	lBinding.Command(model)
	view = model.View()
	assert.NotContains(t, view, "�", "View should not contain replacement character at position 1")
	cleanView = stripAnsi(view)
	assert.Contains(t, cleanView, "żółć", "View should contain 'żółć' at position 1")

	// Move to position 2 (on 'ł')
	lBinding.Command(model)
	view = model.View()
	assert.NotContains(t, view, "�", "View should not contain replacement character at position 2")
	cleanView = stripAnsi(view)
	assert.Contains(t, cleanView, "żółć", "View should contain 'żółć' at position 2")

	// Move to position 3 (on 'ć')
	lBinding.Command(model)
	view = model.View()
	assert.NotContains(t, view, "�", "View should not contain replacement character at position 3")
	cleanView = stripAnsi(view)
	assert.Contains(t, cleanView, "żółć", "View should contain 'żółć' at position 3")

	// Now move back with 'h'
	hBinding := model.registry.FindExact("h", ModeNormal)
	require.NotNil(t, hBinding, "Binding for 'h' not found")

	// Move back to position 2
	hBinding.Command(model)
	view = model.View()
	assert.NotContains(t, view, "�", "View should not contain replacement character moving back to position 2")
	cleanView = stripAnsi(view)
	assert.Contains(t, cleanView, "żółć", "View should contain 'żółć' moving back to position 2")

	// Move back to position 1
	hBinding.Command(model)
	view = model.View()
	assert.NotContains(t, view, "�", "View should not contain replacement character moving back to position 1")
	cleanView = stripAnsi(view)
	assert.Contains(t, cleanView, "żółć", "View should contain 'żółć' moving back to position 1")

	// Move back to position 0 - this was the problematic case in the bug report
	hBinding.Command(model)
	view = model.View()
	assert.NotContains(t, view, "�", "View should not contain replacement character moving back to position 0")
	cleanView = stripAnsi(view)
	assert.Contains(t, cleanView, "żółć", "View should contain 'żółć' moving back to position 0")

	// Success! The bug is fixed - no � characters appear when moving cursor
}

func TestUTF8MultilineWithSyntaxHighlighting(t *testing.T) {
	// Test cursor movement across multiple lines with Polish characters
	content := "żółć\nprąd\nkość"
	editor := NewEditor(
		WithContent(content),
		WithFileName("test.go"),
	)
	model := editor.(*editorModel)

	model.width = 80
	model.height = 20
	model.viewport.Width = 80
	model.viewport.Height = 20
	model.mode = ModeNormal

	// Test moving through all three lines
	lines := []struct {
		row  int
		word string
	}{
		{0, "żółć"},
		{1, "prąd"},
		{2, "kość"},
	}

	for _, line := range lines {
		model.cursor = newCursor(line.row, 0)

		// Move cursor through each character in the line
		lineText := model.buffer.Line(line.row)
		runes := []rune(lineText)

		for col := 0; col < len(runes); col++ {
			model.cursor.Col = col
			view := model.View()

			// No replacement characters
			assert.NotContains(t, view, "�", "View should not contain replacement character at row %d, col %d", line.row, col)

			// Word should be intact
			cleanView := stripAnsi(view)
			assert.Contains(t, cleanView, line.word, "View should contain '%s' at row %d, col %d", line.word, line.row, col)
		}
	}
}

func TestEmojiDisplay(t *testing.T) {
	// Test displaying emojis - these are 4-byte UTF-8 characters
	// Testing: 😀 (U+1F600) = F0 9F 98 80 (4 bytes)
	content := "Hello 😀 World"
	editor := NewEditor(WithContent(content))
	model := editor.(*editorModel)

	model.width = 80
	model.height = 20
	model.viewport.Width = 80
	model.viewport.Height = 20
	model.mode = ModeNormal

	// Get the line and verify it has correct length
	line := model.buffer.Line(0)
	runes := []rune(line)

	// "Hello 😀 World" = 5 + 1 + 1 + 1 + 5 = 13 runes
	assert.Equal(t, 13, len(runes), "Line should have 13 runes (emoji counts as 1 rune)")

	// Verify the emoji is at position 6
	assert.Equal(t, '😀', runes[6], "Character at position 6 should be emoji 😀")

	// Test cursor movement through text with emoji
	positions := []struct {
		col         int
		description string
	}{
		{0, "H"},
		{5, "space before emoji"},
		{6, "😀 emoji"},
		{7, "space after emoji"},
		{12, "d at end"},
	}

	for _, pos := range positions {
		model.cursor.Col = pos.col
		view := model.View()

		// Should not have replacement characters
		assert.NotContains(t, view, "�", "View should not have replacement character at %s (pos %d)", pos.description, pos.col)

		// Full content should be preserved
		cleanView := stripAnsi(view)
		assert.Contains(t, cleanView, "😀", "View should contain emoji at %s (pos %d)", pos.description, pos.col)
		assert.Contains(t, cleanView, "Hello", "View should contain 'Hello' at %s (pos %d)", pos.description, pos.col)
		assert.Contains(t, cleanView, "World", "View should contain 'World' at %s (pos %d)", pos.description, pos.col)
	}
}

func TestEmojiCursorNavigation(t *testing.T) {
	// Test navigating through emojis with h/l commands
	content := "😀😎🎉"  // Three emojis
	editor := NewEditor(WithContent(content))
	model := editor.(*editorModel)

	model.width = 80
	model.height = 20
	model.viewport.Width = 80
	model.viewport.Height = 20
	model.mode = ModeNormal

	// Verify line has 3 runes
	line := model.buffer.Line(0)
	runes := []rune(line)
	assert.Equal(t, 3, len(runes), "Three emojis should be 3 runes")
	assert.Equal(t, '😀', runes[0], "First emoji should be 😀")
	assert.Equal(t, '😎', runes[1], "Second emoji should be 😎")
	assert.Equal(t, '🎉', runes[2], "Third emoji should be 🎉")

	// Navigate through emojis
	model.cursor = newCursor(0, 0)

	lBinding := model.registry.FindExact("l", ModeNormal)
	require.NotNil(t, lBinding, "Binding for 'l' not found")

	// Start at 😀
	view := model.View()
	assert.NotContains(t, view, "�", "View should not have � at emoji 😀")

	// Move to 😎
	lBinding.Command(model)
	assert.Equal(t, 1, model.cursor.Col, "Cursor should be at position 1")
	view = model.View()
	assert.NotContains(t, view, "�", "View should not have � at emoji 😎")

	// Move to 🎉
	lBinding.Command(model)
	assert.Equal(t, 2, model.cursor.Col, "Cursor should be at position 2")
	view = model.View()
	assert.NotContains(t, view, "�", "View should not have � at emoji 🎉")

	// Verify all emojis are intact in final view
	cleanView := stripAnsi(view)
	assert.Contains(t, cleanView, "😀😎🎉", "All emojis should be intact")
}

func TestEmojiEditing(t *testing.T) {
	// Test inserting and deleting emojis
	editor := NewEditor(WithContent("Hello World"))
	model := editor.(*editorModel)

	model.width = 80
	model.height = 20
	model.viewport.Width = 80
	model.viewport.Height = 20

	// Insert emoji in insert mode
	model.mode = ModeInsert
	model.cursor = newCursor(0, 5)  // After "Hello"

	// Insert space and emoji
	insertCharacter(model, " ")
	insertCharacter(model, "😀")

	line := model.buffer.Line(0)
	assert.Contains(t, line, "😀", "Line should contain inserted emoji")

	// Verify structure: "Hello 😀 World"
	runes := []rune(line)
	assert.Equal(t, '😀', runes[6], "Emoji should be at position 6")

	// Test Enter key with emoji
	model.cursor.Col = 7  // After emoji
	handleInsertEnterKey(model)

	assert.Equal(t, 2, model.buffer.lineCount(), "Should have 2 lines after Enter")
	line1 := model.buffer.Line(0)
	line2 := model.buffer.Line(1)

	assert.Contains(t, line1, "😀", "First line should contain emoji")
	assert.Contains(t, line2, "World", "Second line should contain 'World'")
}

func TestMixedUTF8Content(t *testing.T) {
	// Test mixing emojis with regular text
	content := "Hello 😀 world 🎉 test!"
	editor := NewEditor(WithContent(content))
	model := editor.(*editorModel)

	model.width = 80
	model.height = 20
	model.viewport.Width = 80
	model.viewport.Height = 20
	model.mode = ModeNormal

	line := model.buffer.Line(0)
	runes := []rune(line)

	// Verify correct character count
	// "Hello 😀 world 🎉 test!"
	// = 5 + 1 + 1 + 1 + 5 + 1 + 1 + 1 + 5 = 21 characters
	assert.Equal(t, 21, len(runes), "Mixed content should have 21 runes")

	// Test cursor at various positions
	testPositions := []int{0, 5, 6, 7, 11, 12, 13, 20}

	for _, pos := range testPositions {
		if pos >= len(runes) {
			continue
		}
		model.cursor.Col = pos
		view := model.View()

		// No corruption
		assert.NotContains(t, view, "�", "No corruption at position %d", pos)

		// All content intact
		cleanView := stripAnsi(view)
		assert.Contains(t, cleanView, "Hello", "Should contain 'Hello'")
		assert.Contains(t, cleanView, "😀", "Should contain '😀'")
		assert.Contains(t, cleanView, "world", "Should contain 'world'")
		assert.Contains(t, cleanView, "🎉", "Should contain '🎉'")
		assert.Contains(t, cleanView, "test", "Should contain 'test'")
	}
}
