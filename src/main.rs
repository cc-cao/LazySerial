use std::io::{Read, Write};
use std::sync::mpsc::{self, Receiver, Sender};
use std::time::Duration;

use crossterm::cursor::{EnableBlinking, SetCursorStyle, Show};
use crossterm::event::{
    self, DisableMouseCapture, EnableMouseCapture, Event, KeyCode, KeyEvent, KeyEventKind,
    KeyModifiers, MouseEventKind,
};
use crossterm::execute;
use ratatui::layout::{Constraint, Direction, Layout, Rect};
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, BorderType, Borders, Clear, List, ListItem, Paragraph, Wrap};
use ratatui::{DefaultTerminal, Frame};
use serialport::{DataBits, Parity, SerialPort, StopBits};

const COLOR_FG: Color = Color::Rgb(205, 214, 244);
const COLOR_BORDER_ACTIVE: Color = Color::Rgb(166, 227, 161);
const COLOR_BORDER_INACTIVE: Color = Color::Rgb(108, 112, 134);
const COLOR_ACCENT: Color = Color::Rgb(137, 180, 250);
const COLOR_HINT: Color = Color::Rgb(148, 226, 213);
const COLOR_LINE_NO: Color = Color::Rgb(137, 180, 250);
const COLOR_RED: Color = Color::Rgb(243, 139, 168);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum InputMode {
    Terminal,
    Command,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum PanelFocus {
    Serial,
    Display,
    Terminal,
    Command,
}

impl PanelFocus {
    fn title(self) -> &'static str {
        match self {
            PanelFocus::Serial => "Serial Config",
            PanelFocus::Display => "Display Config",
            PanelFocus::Terminal => "Terminal View",
            PanelFocus::Command => "Command Input",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum DisplayFormat {
    Ascii,
    Hex,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum SendFormat {
    Ascii,
    Hex,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum LineEnding {
    Lf,
    Cr,
    CrLf,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum PopupKind {
    Port,
    Baud,
    DataBits,
    StopBits,
    Parity,
    LineEnding,
}

struct ChoicePopup {
    kind: PopupKind,
    selected: usize,
}

struct SerialConnection {
    tx: Sender<Vec<u8>>,
}

struct App {
    running: bool,
    mode: InputMode,
    focus: PanelFocus,
    show_help_popup: bool,
    choice_popup: Option<ChoicePopup>,
    custom_baud_input: Option<String>,

    serial_cursor: usize,
    display_cursor: usize,

    command_input: String,
    command_history: Vec<String>,
    command_history_index: Option<usize>,
    command_history_draft: Option<String>,
    terminal_input: String,
    logs: Vec<String>,
    next_line_no: usize,
    log_scroll_top: Option<usize>,

    ports: Vec<String>,
    selected_port: usize,
    baud_presets: Vec<u32>,
    selected_baud: u32,
    custom_baud: Option<u32>,
    selected_data_bits: DataBits,
    selected_stop_bits: StopBits,
    selected_parity: Parity,
    dtr_enabled: bool,
    rts_enabled: bool,
    connected: bool,

    show_line_numbers: bool,
    display_format: DisplayFormat,
    send_format: SendFormat,
    auto_wrap: bool,
    line_ending: LineEnding,

    serial_conn: Option<SerialConnection>,
    rx_serial_data: Receiver<Vec<u8>>,
    tx_serial_data: Sender<Vec<u8>>,
    rx_write_err: Receiver<String>,
    tx_write_err: Sender<String>,
}

impl App {
    fn new() -> Self {
        let (tx_serial_data, rx_serial_data) = mpsc::channel();
        let (tx_write_err, rx_write_err) = mpsc::channel();
        Self {
            running: true,
            mode: InputMode::Terminal,
            focus: PanelFocus::Serial,
            show_help_popup: false,
            choice_popup: None,
            custom_baud_input: None,

            serial_cursor: 0,
            display_cursor: 0,

            command_input: String::new(),
            command_history: Vec::new(),
            command_history_index: None,
            command_history_draft: None,
            terminal_input: String::new(),
            logs: vec![],
            next_line_no: 1,
            log_scroll_top: None,

            ports: collect_port_names(),
            selected_port: 0,
            baud_presets: vec![9600, 19200, 38400, 57600, 115200, 230400, 460800, 921600],
            selected_baud: 115200,
            custom_baud: None,
            selected_data_bits: DataBits::Eight,
            selected_stop_bits: StopBits::One,
            selected_parity: Parity::None,
            dtr_enabled: false,
            rts_enabled: false,
            connected: false,

            show_line_numbers: false,
            display_format: DisplayFormat::Ascii,
            send_format: SendFormat::Ascii,
            auto_wrap: true,
            line_ending: LineEnding::Lf,

            serial_conn: None,
            rx_serial_data,
            tx_serial_data,
            rx_write_err,
            tx_write_err,
        }
    }

    fn run(&mut self, terminal: &mut DefaultTerminal) -> std::io::Result<()> {
        while self.running {
            self.drain_serial_events();
            terminal.draw(|frame| self.draw(frame))?;

            if event::poll(Duration::from_millis(30))? {
                match event::read()? {
                    Event::Key(key) if key.kind == KeyEventKind::Press => {
                        self.handle_key(key);
                    }
                    Event::Mouse(mouse) => {
                        self.handle_mouse(mouse.kind, mouse.column, mouse.row);
                    }
                    _ => {}
                }
            }
        }
        Ok(())
    }

    fn draw(&self, frame: &mut Frame) {
        let root = frame.area();
        let rows = Layout::default()
            .direction(Direction::Vertical)
            .constraints([Constraint::Min(5), Constraint::Length(2)])
            .split(root);
        let columns = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([Constraint::Percentage(20), Constraint::Percentage(80)])
            .split(rows[0]);

        self.draw_left_panel(frame, columns[0]);
        self.draw_right_panel(frame, columns[1]);
        self.draw_footer(frame, rows[1]);

        if self.show_help_popup {
            self.draw_help_popup(frame, root);
        } else if self.custom_baud_input.is_some() {
            self.draw_custom_baud_popup(frame, root);
        } else if self.choice_popup.is_some() {
            self.draw_choice_popup(frame, root);
        }
    }

    fn draw_left_panel(&self, frame: &mut Frame, area: Rect) {
        let sections = Layout::default()
            .direction(Direction::Vertical)
            .constraints([Constraint::Percentage(50), Constraint::Percentage(50)])
            .split(area);

        let selected_port = self
            .ports
            .get(self.selected_port)
            .map_or("<No port>", String::as_str);
        let baud = self.selected_baud;
        let data_bits = data_bits_label(self.selected_data_bits);
        let stop_bits = stop_bits_label(self.selected_stop_bits);
        let parity = parity_label(self.selected_parity);

        let serial_items = vec![
            self.config_item(
                PanelFocus::Serial,
                self.serial_cursor == 0,
                format!("Port: {selected_port}"),
            ),
            self.config_item(
                PanelFocus::Serial,
                self.serial_cursor == 1,
                format!("Baud: {baud}"),
            ),
            self.config_item(
                PanelFocus::Serial,
                self.serial_cursor == 2,
                format!("Data Bits: {data_bits}"),
            ),
            self.config_item(
                PanelFocus::Serial,
                self.serial_cursor == 3,
                format!("Stop Bits: {stop_bits}"),
            ),
            self.config_item(
                PanelFocus::Serial,
                self.serial_cursor == 4,
                format!("Parity: {parity}"),
            ),
            self.config_item(
                PanelFocus::Serial,
                self.serial_cursor == 5,
                format!("DTR: {}", yes_no(self.dtr_enabled)),
            ),
            self.config_item(
                PanelFocus::Serial,
                self.serial_cursor == 6,
                format!("RTS: {}", yes_no(self.rts_enabled)),
            ),
            self.config_item(
                PanelFocus::Serial,
                self.serial_cursor == 7,
                format!(
                    "Connection: {}",
                    if self.connected {
                        "Connected"
                    } else {
                        "Disconnected"
                    }
                ),
            ),
        ];
        let serial_list = List::new(serial_items).block(self.panel_block(
            PanelFocus::Serial,
            format!("[1] {}", PanelFocus::Serial.title()),
        ));
        frame.render_widget(serial_list, sections[0]);

        let format_text = match self.display_format {
            DisplayFormat::Ascii => "ASCII",
            DisplayFormat::Hex => "HEX",
        };
        let mode_text = match self.mode {
            InputMode::Terminal => "Terminal",
            InputMode::Command => "Command",
        };
        let send_format_text = match self.send_format {
            SendFormat::Ascii => "ASCII",
            SendFormat::Hex => "HEX",
        };
        let line_ending = line_ending_label(self.line_ending);
        let display_items = vec![
            self.config_item(
                PanelFocus::Display,
                self.display_cursor == 0,
                format!("Mode: {mode_text}"),
            ),
            self.config_item(
                PanelFocus::Display,
                self.display_cursor == 1,
                format!("Line numbers: {}", yes_no(self.show_line_numbers)),
            ),
            self.config_item(
                PanelFocus::Display,
                self.display_cursor == 2,
                format!("Line ending: {line_ending}"),
            ),
            self.config_item(
                PanelFocus::Display,
                self.display_cursor == 3,
                format!("Auto wrap: {}", yes_no(self.auto_wrap)),
            ),
            self.config_item(
                PanelFocus::Display,
                self.display_cursor == 4,
                format!("View mode: {format_text}"),
            ),
            self.config_item(
                PanelFocus::Display,
                self.display_cursor == 5,
                format!("Send format: {send_format_text}"),
            ),
        ];
        let display_list = List::new(display_items).block(self.panel_block(
            PanelFocus::Display,
            format!("[2] {}", PanelFocus::Display.title()),
        ));
        frame.render_widget(display_list, sections[1]);
    }

    fn config_item(&self, panel: PanelFocus, selected: bool, content: String) -> ListItem<'static> {
        let is_focused_item = self.focus == panel
            && selected
            && self.choice_popup.is_none()
            && self.custom_baud_input.is_none();
        let marker = if is_focused_item { ">" } else { " " };
        let style = if is_focused_item {
            Style::default()
                .fg(COLOR_BORDER_ACTIVE)
                .add_modifier(Modifier::BOLD | Modifier::REVERSED)
        } else {
            Style::default().fg(COLOR_FG)
        };

        ListItem::new(Line::from(format!("{marker} {content}"))).style(style)
    }

    fn draw_right_panel(&self, frame: &mut Frame, area: Rect) {
        match self.mode {
            InputMode::Terminal => {
                let block = self.panel_block(
                    PanelFocus::Terminal,
                    format!("[3] {}", PanelFocus::Terminal.title()),
                );
                let inner = block.inner(area);
                frame.render_widget(block, area);

                if inner.width > 0 && inner.height > 0 {
                    let plain_lines = self.terminal_plain_lines();
                    let styled_lines = self.styled_lines_from_strings(&plain_lines);
                    let (scroll_y, cursor_col, cursor_row) =
                        self.terminal_viewport_metrics(&plain_lines, inner.width, inner.height);

                    let paragraph = Paragraph::new(styled_lines)
                        .style(Style::default().fg(COLOR_FG))
                        .wrap(Wrap {
                            trim: !self.auto_wrap,
                        })
                        .scroll((scroll_y, 0));
                    frame.render_widget(paragraph, inner);

                    if self.focus == PanelFocus::Terminal {
                        let max_x = inner.x + inner.width.saturating_sub(1);
                        let max_y = inner.y + inner.height.saturating_sub(1);
                        let cursor_x = (inner.x + cursor_col).min(max_x);
                        let cursor_y = (inner.y + cursor_row).min(max_y);
                        frame.set_cursor_position((cursor_x, cursor_y));
                    }
                }
            }
            InputMode::Command => {
                let sections = Layout::default()
                    .direction(Direction::Vertical)
                    .constraints([Constraint::Percentage(75), Constraint::Percentage(25)])
                    .split(area);

                self.draw_log_view(frame, sections[0]);

                let input = Paragraph::new(self.command_input.as_str())
                    .block(self.panel_block(
                        PanelFocus::Command,
                        format!("[4] {} Enter send, Esc cancel", PanelFocus::Command.title()),
                    ))
                    .style(Style::default().fg(COLOR_FG));
                frame.render_widget(Clear, sections[1]);
                frame.render_widget(input, sections[1]);
                if self.focus == PanelFocus::Command {
                    let cursor_x = sections[1].x + self.command_input.chars().count() as u16 + 1;
                    let cursor_y = sections[1].y + 1;
                    frame.set_cursor_position((cursor_x, cursor_y));
                }
            }
        }
    }

    fn draw_log_view(&self, frame: &mut Frame, area: Rect) {
        let block = self.panel_block(
            PanelFocus::Terminal,
            format!("[3] {}", PanelFocus::Terminal.title()),
        );
        let inner = block.inner(area);
        let max_scroll = self.max_scroll(&self.logs, inner.width, inner.height);
        let scroll_y = self.current_scroll(max_scroll) as u16;
        let paragraph = Paragraph::new(self.log_lines())
            .block(block)
            .style(Style::default().fg(COLOR_FG))
            .wrap(Wrap {
                trim: !self.auto_wrap,
            })
            .scroll((scroll_y, 0));
        frame.render_widget(paragraph, area);
    }

    fn log_lines(&self) -> Vec<Line<'static>> {
        self.styled_lines_from_strings(&self.logs)
    }

    fn terminal_plain_lines(&self) -> Vec<String> {
        if self.logs.is_empty() {
            return vec![self.terminal_input.clone()];
        }

        let mut lines = self.logs.clone();
        if !self.terminal_input.is_empty() {
            if let Some(last) = lines.last_mut() {
                last.push_str(&self.terminal_input);
            }
        }
        lines
    }

    fn styled_lines_from_strings(&self, lines: &[String]) -> Vec<Line<'static>> {
        lines
            .iter()
            .map(|l| {
                if let Some((line_no, rest)) = l.split_once(" | ") {
                    if line_no.chars().all(|c| c.is_ascii_digit()) {
                        return Line::from(vec![
                            Span::styled(line_no.to_string(), Style::default().fg(COLOR_LINE_NO)),
                            Span::raw(" | "),
                            Span::raw(rest.to_string()),
                        ]);
                    }
                }
                Line::from(Span::raw(l.to_string()))
            })
            .collect()
    }

    fn terminal_viewport_metrics(
        &self,
        lines: &[String],
        width: u16,
        height: u16,
    ) -> (u16, u16, u16) {
        if width == 0 || height == 0 {
            return (0, 0, 0);
        }

        let (row, col, total_rows) = self.content_layout_metrics(lines, width);
        let max_scroll = total_rows.saturating_sub(height as usize);
        let scroll_y = self.current_scroll(max_scroll) as u16;
        let visible_row = row
            .saturating_sub(scroll_y as usize)
            .min(height as usize - 1) as u16;
        let visible_col = col.min(width as usize - 1) as u16;

        (scroll_y, visible_col, visible_row)
    }

    fn max_scroll(&self, lines: &[String], width: u16, height: u16) -> usize {
        if width == 0 || height == 0 {
            return 0;
        }
        let (_, _, total_rows) = self.content_layout_metrics(lines, width);
        total_rows.saturating_sub(height as usize)
    }

    fn current_scroll(&self, max_scroll: usize) -> usize {
        self.log_scroll_top.unwrap_or(max_scroll).min(max_scroll)
    }

    fn content_layout_metrics(&self, lines: &[String], width: u16) -> (usize, usize, usize) {
        if width == 0 {
            return (0, 0, 1);
        }

        let content = lines.join("\n");
        let mut row: usize = 0;
        let mut col: usize = 0;
        let wrap_width = width as usize;

        for ch in content.chars() {
            if ch == '\n' {
                row += 1;
                col = 0;
                continue;
            }

            if self.auto_wrap && col >= wrap_width {
                row += 1;
                col = 0;
            }
            col += 1;
        }

        (row, col, row + 1)
    }

    fn draw_footer(&self, frame: &mut Frame, area: Rect) {
        let footer_cols = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([Constraint::Min(10), Constraint::Length(20)])
            .split(area);

        let status_text = if self.connected {
            format!(
                "CONNECTED {} @ {}",
                self.current_port_label(),
                self.selected_baud
            )
        } else {
            "DISCONNECTED".to_string()
        };
        let status_style = if self.connected {
            Style::default()
                .fg(COLOR_BORDER_ACTIVE)
                .add_modifier(Modifier::BOLD)
        } else {
            Style::default().fg(COLOR_RED).add_modifier(Modifier::BOLD)
        };

        let mut spans = vec![
            Span::styled(format!(" {status_text} "), status_style),
            Span::raw(" "),
            keycap("Ctrl+1-4"),
            hint("jump"),
            keycap("Ctrl+A/S/D/F"),
            hint("jump"),
            keycap("?"),
            hint("help"),
            keycap("Ctrl+q"),
            hint("quit"),
            separator(),
        ];

        for (idx, (key, desc)) in self.focus_shortcuts().iter().enumerate() {
            spans.push(keycap(key));
            spans.push(hint(desc));
            if idx + 1 != self.focus_shortcuts().len() {
                spans.push(Span::raw(" "));
            }
        }

        let footer = Paragraph::new(Line::from(spans))
            .style(Style::default().fg(COLOR_FG))
            .wrap(Wrap { trim: true });
        frame.render_widget(footer, footer_cols[0]);

        let version = Paragraph::new(format!("v{}", env!("CARGO_PKG_VERSION")))
            .style(Style::default().fg(Color::Yellow))
            .alignment(ratatui::layout::Alignment::Right);
        frame.render_widget(version, footer_cols[1]);
    }

    fn draw_help_popup(&self, frame: &mut Frame, root: Rect) {
        let popup = centered_rect(70, 65, root);
        frame.render_widget(Clear, popup);

        let help_lines = vec![
            Line::from("Global:"),
            Line::from("  Ctrl+1 2 3 4     Jump panel focus"),
            Line::from("  Ctrl+A S D F     Jump panel focus (fallback)"),
            Line::from("  ?                Toggle this help"),
            Line::from("  Ctrl+q           Quit"),
            Line::from(""),
            Line::from("Config panels ([1],[2]):"),
            Line::from("  j / k or Up/Down Move selected item"),
            Line::from("  Enter            Open list / execute item"),
            Line::from(""),
            Line::from("Choice popup:"),
            Line::from("  j / k or Up/Down Move option"),
            Line::from("  Enter            Confirm"),
            Line::from("  Esc              Cancel"),
            Line::from(""),
            Line::from("[4] Command Input:"),
            Line::from("  Enter            Send command"),
            Line::from("  Up / Down        History"),
            Line::from("  Esc              Back to terminal mode"),
            Line::from("  Backspace        Delete char"),
        ];

        let popup_widget = Paragraph::new(help_lines)
            .block(
                Block::default()
                    .title(Span::styled(
                        "Shortcut Help (?)",
                        Style::default()
                            .fg(COLOR_ACCENT)
                            .add_modifier(Modifier::BOLD),
                    ))
                    .borders(Borders::ALL)
                    .border_type(BorderType::Rounded)
                    .border_style(
                        Style::default()
                            .fg(COLOR_BORDER_ACTIVE)
                            .add_modifier(Modifier::BOLD),
                    ),
            )
            .style(Style::default().fg(COLOR_FG))
            .wrap(Wrap { trim: true });

        frame.render_widget(popup_widget, popup);
    }

    fn draw_choice_popup(&self, frame: &mut Frame, root: Rect) {
        let Some(popup) = &self.choice_popup else {
            return;
        };

        let popup_area = centered_rect(46, 55, root);
        frame.render_widget(Clear, popup_area);

        let options = self.popup_options(popup.kind);
        let items: Vec<ListItem> = options
            .iter()
            .enumerate()
            .map(|(idx, opt)| {
                let active = idx == popup.selected;
                let style = if active {
                    Style::default()
                        .fg(COLOR_BORDER_ACTIVE)
                        .add_modifier(Modifier::BOLD | Modifier::REVERSED)
                } else {
                    Style::default().fg(COLOR_FG)
                };
                let marker = if active { ">" } else { " " };
                ListItem::new(format!("{marker} {opt}")).style(style)
            })
            .collect();

        let list = List::new(items).block(
            Block::default()
                .title(Span::styled(
                    format!("{} Enter confirm, Esc cancel", self.popup_title(popup.kind)),
                    Style::default()
                        .fg(COLOR_ACCENT)
                        .add_modifier(Modifier::BOLD),
                ))
                .borders(Borders::ALL)
                .border_type(BorderType::Rounded)
                .border_style(
                    Style::default()
                        .fg(COLOR_BORDER_ACTIVE)
                        .add_modifier(Modifier::BOLD),
                ),
        );

        frame.render_widget(list, popup_area);
    }

    fn draw_custom_baud_popup(&self, frame: &mut Frame, root: Rect) {
        let Some(input) = &self.custom_baud_input else {
            return;
        };

        let popup_area = centered_rect(44, 30, root);
        frame.render_widget(Clear, popup_area);

        let paragraph = Paragraph::new(input.as_str())
            .block(
                Block::default()
                    .title(Span::styled(
                        "Custom Baud Enter value, Enter save, Esc cancel",
                        Style::default()
                            .fg(COLOR_ACCENT)
                            .add_modifier(Modifier::BOLD),
                    ))
                    .borders(Borders::ALL)
                    .border_type(BorderType::Rounded)
                    .border_style(
                        Style::default()
                            .fg(COLOR_BORDER_ACTIVE)
                            .add_modifier(Modifier::BOLD),
                    ),
            )
            .style(Style::default().fg(COLOR_FG));

        frame.render_widget(paragraph, popup_area);
        let cursor_x = popup_area.x + input.chars().count() as u16 + 1;
        let cursor_y = popup_area.y + 1;
        frame.set_cursor_position((cursor_x, cursor_y));
    }

    fn panel_block<T: Into<String>>(&self, panel: PanelFocus, title: T) -> Block<'_> {
        let active = self.focus == panel;
        let border_style = if active {
            Style::default()
                .fg(COLOR_BORDER_ACTIVE)
                .add_modifier(Modifier::BOLD)
        } else {
            Style::default().fg(COLOR_BORDER_INACTIVE)
        };
        let title_style = if active {
            Style::default()
                .fg(COLOR_BORDER_ACTIVE)
                .add_modifier(Modifier::BOLD)
        } else {
            Style::default().fg(COLOR_ACCENT)
        };

        Block::default()
            .title(Span::styled(title.into(), title_style))
            .borders(Borders::ALL)
            .border_type(BorderType::Rounded)
            .border_style(border_style)
            .style(Style::default())
    }

    fn focus_shortcuts(&self) -> &'static [(&'static str, &'static str)] {
        if self.choice_popup.is_some() {
            return &[
                ("j/k or Up/Down", "select"),
                ("Enter", "confirm"),
                ("Esc", "cancel"),
            ];
        }
        if self.custom_baud_input.is_some() {
            return &[("0-9", "input"), ("Enter", "save"), ("Esc", "cancel")];
        }

        match self.focus {
            PanelFocus::Serial => &[("j/k or Up/Down", "move item"), ("Enter", "open/select")],
            PanelFocus::Display => &[("j/k or Up/Down", "move item"), ("Enter", "toggle/select")],
            PanelFocus::Terminal => &[("Wheel or Ctrl+Up/Down", "scroll"), ("Ctrl+j/k", "scroll")],
            PanelFocus::Command => &[
                ("Enter", "send"),
                ("Up/Down", "history"),
                ("Esc", "cancel"),
                ("Backspace", "delete"),
            ],
        }
    }

    fn handle_key(&mut self, key: KeyEvent) {
        let code = key.code;
        let modifiers = key.modifiers;

        if self.show_help_popup {
            if matches!(code, KeyCode::Esc | KeyCode::Char('?')) {
                self.show_help_popup = false;
            }
            return;
        }

        if self.custom_baud_input.is_some() {
            self.handle_custom_baud_input_key(code);
            return;
        }

        if self.choice_popup.is_some() {
            self.handle_choice_popup_key(code);
            return;
        }

        if code == KeyCode::Char('q') && modifiers.contains(KeyModifiers::CONTROL) {
            self.running = false;
            return;
        }

        let ctrl_jump = modifiers.contains(KeyModifiers::CONTROL);

        match code {
            KeyCode::Char('?') => {
                self.show_help_popup = true;
                return;
            }
            KeyCode::Char('1') if ctrl_jump => {
                self.focus = PanelFocus::Serial;
                return;
            }
            KeyCode::Char('2') if ctrl_jump => {
                self.focus = PanelFocus::Display;
                return;
            }
            KeyCode::Char('3') if ctrl_jump => {
                self.focus = PanelFocus::Terminal;
                return;
            }
            KeyCode::Char('4') if ctrl_jump => {
                self.focus = PanelFocus::Command;
                self.mode = InputMode::Command;
                return;
            }
            KeyCode::Char('a') if ctrl_jump => {
                self.focus = PanelFocus::Serial;
                return;
            }
            KeyCode::Char('s') if ctrl_jump => {
                self.focus = PanelFocus::Display;
                return;
            }
            KeyCode::Char('d') if ctrl_jump => {
                self.focus = PanelFocus::Terminal;
                return;
            }
            KeyCode::Char('f') if ctrl_jump => {
                self.focus = PanelFocus::Command;
                self.mode = InputMode::Command;
                return;
            }
            _ => {}
        }

        match self.focus {
            PanelFocus::Serial => self.handle_serial_key(code),
            PanelFocus::Display => self.handle_display_key(code),
            PanelFocus::Terminal => self.handle_terminal_key(key),
            PanelFocus::Command => self.handle_command_key(code),
        }
    }

    fn handle_mouse(&mut self, kind: MouseEventKind, column: u16, row: u16) {
        if self.show_help_popup || self.custom_baud_input.is_some() || self.choice_popup.is_some() {
            return;
        }

        let (width, height) = crossterm::terminal::size().unwrap_or((120, 40));
        let root = Rect::new(0, 0, width, height);
        let rows = Layout::default()
            .direction(Direction::Vertical)
            .constraints([Constraint::Min(5), Constraint::Length(2)])
            .split(root);
        let columns = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([Constraint::Percentage(20), Constraint::Percentage(80)])
            .split(rows[0]);
        let left_sections = Layout::default()
            .direction(Direction::Vertical)
            .constraints([Constraint::Percentage(50), Constraint::Percentage(50)])
            .split(columns[0]);

        if rect_contains(left_sections[0], column, row) {
            self.focus = PanelFocus::Serial;
            return;
        }
        if rect_contains(left_sections[1], column, row) {
            self.focus = PanelFocus::Display;
            return;
        }

        let panel3_area = match self.mode {
            InputMode::Terminal => columns[1],
            InputMode::Command => Layout::default()
                .direction(Direction::Vertical)
                .constraints([Constraint::Percentage(75), Constraint::Percentage(25)])
                .split(columns[1])[0],
        };
        if rect_contains(panel3_area, column, row) {
            if matches!(kind, MouseEventKind::ScrollUp) {
                self.scroll_up(3);
                return;
            }
            if matches!(kind, MouseEventKind::ScrollDown) {
                self.scroll_down(3);
                return;
            }
        }

        if !matches!(kind, MouseEventKind::Down(_)) {
            return;
        }

        match self.mode {
            InputMode::Terminal => {
                if rect_contains(columns[1], column, row) {
                    self.focus = PanelFocus::Terminal;
                }
            }
            InputMode::Command => {
                let right_sections = Layout::default()
                    .direction(Direction::Vertical)
                    .constraints([Constraint::Percentage(75), Constraint::Percentage(25)])
                    .split(columns[1]);
                if rect_contains(right_sections[1], column, row) {
                    self.focus = PanelFocus::Command;
                } else if rect_contains(right_sections[0], column, row) {
                    self.focus = PanelFocus::Terminal;
                }
            }
        }
    }

    fn handle_choice_popup_key(&mut self, code: KeyCode) {
        let Some((kind, current_selected)) =
            self.choice_popup.as_ref().map(|p| (p.kind, p.selected))
        else {
            return;
        };

        let len = self.popup_options(kind).len();
        match code {
            KeyCode::Esc => self.choice_popup = None,
            KeyCode::Char('j') | KeyCode::Down => {
                if len > 0 {
                    let next = (current_selected + 1) % len;
                    if let Some(popup) = &mut self.choice_popup {
                        popup.selected = next;
                    }
                }
            }
            KeyCode::Char('k') | KeyCode::Up => {
                if len > 0 {
                    let next = if current_selected == 0 {
                        len - 1
                    } else {
                        current_selected - 1
                    };
                    if let Some(popup) = &mut self.choice_popup {
                        popup.selected = next;
                    }
                }
            }
            KeyCode::Enter => {
                self.apply_popup_selection(kind, current_selected);
                self.choice_popup = None;
            }
            _ => {}
        }
    }

    fn handle_custom_baud_input_key(&mut self, code: KeyCode) {
        match code {
            KeyCode::Esc => self.custom_baud_input = None,
            KeyCode::Backspace => {
                if let Some(input) = &mut self.custom_baud_input {
                    input.pop();
                }
            }
            KeyCode::Enter => {
                let value = self
                    .custom_baud_input
                    .as_deref()
                    .unwrap_or_default()
                    .trim()
                    .parse::<u32>();
                match value {
                    Ok(v) if v > 0 => {
                        self.custom_baud = Some(v);
                        self.selected_baud = v;
                        self.custom_baud_input = None;
                        self.push_local_log(&format!("Custom baud set to {v}"));
                    }
                    _ => {
                        self.push_local_log("Invalid baud. Please input a positive integer.");
                    }
                }
            }
            KeyCode::Char(ch) if ch.is_ascii_digit() => {
                if let Some(input) = &mut self.custom_baud_input {
                    if input.len() < 9 {
                        input.push(ch);
                    }
                }
            }
            _ => {}
        }
    }

    fn toggle_mode(&mut self) {
        if self.mode == InputMode::Command {
            self.mode = InputMode::Terminal;
        } else {
            self.mode = InputMode::Command;
        }
    }

    fn selected_baud_is_custom(&self) -> bool {
        self.custom_baud == Some(self.selected_baud)
    }

    fn current_port_label(&self) -> &str {
        self.ports
            .get(self.selected_port)
            .map_or("<No port>", String::as_str)
    }

    fn handle_serial_key(&mut self, code: KeyCode) {
        const SERIAL_ITEMS: usize = 8;
        match code {
            KeyCode::Char('j') | KeyCode::Down => {
                self.serial_cursor = (self.serial_cursor + 1) % SERIAL_ITEMS;
            }
            KeyCode::Char('k') | KeyCode::Up => {
                self.serial_cursor = if self.serial_cursor == 0 {
                    SERIAL_ITEMS - 1
                } else {
                    self.serial_cursor - 1
                };
            }
            KeyCode::Enter => self.activate_serial_item(),
            _ => {}
        }
    }

    fn activate_serial_item(&mut self) {
        match self.serial_cursor {
            0 => {
                self.ports = collect_port_names();
                if self.selected_port >= self.ports.len() {
                    self.selected_port = 0;
                }
                if self.ports.is_empty() {
                    self.push_local_log("No available serial ports.");
                } else {
                    self.choice_popup = Some(ChoicePopup {
                        kind: PopupKind::Port,
                        selected: self.selected_port.min(self.ports.len().saturating_sub(1)),
                    });
                }
            }
            1 => {
                let selected = if self.selected_baud_is_custom() {
                    self.baud_presets.len()
                } else {
                    self.baud_presets
                        .iter()
                        .position(|&b| b == self.selected_baud)
                        .unwrap_or(0)
                };
                self.choice_popup = Some(ChoicePopup {
                    kind: PopupKind::Baud,
                    selected,
                });
            }
            2 => {
                self.choice_popup = Some(ChoicePopup {
                    kind: PopupKind::DataBits,
                    selected: selected_data_bits_index(self.selected_data_bits),
                });
            }
            3 => {
                self.choice_popup = Some(ChoicePopup {
                    kind: PopupKind::StopBits,
                    selected: selected_stop_bits_index(self.selected_stop_bits),
                });
            }
            4 => {
                self.choice_popup = Some(ChoicePopup {
                    kind: PopupKind::Parity,
                    selected: selected_parity_index(self.selected_parity),
                });
            }
            5 => {
                self.dtr_enabled = !self.dtr_enabled;
            }
            6 => {
                self.rts_enabled = !self.rts_enabled;
            }
            7 => self.toggle_connection(),
            _ => {}
        }
    }

    fn handle_display_key(&mut self, code: KeyCode) {
        const DISPLAY_ITEMS: usize = 6;
        match code {
            KeyCode::Char('j') | KeyCode::Down => {
                self.display_cursor = (self.display_cursor + 1) % DISPLAY_ITEMS;
            }
            KeyCode::Char('k') | KeyCode::Up => {
                self.display_cursor = if self.display_cursor == 0 {
                    DISPLAY_ITEMS - 1
                } else {
                    self.display_cursor - 1
                };
            }
            KeyCode::Enter => self.activate_display_item(),
            _ => {}
        }
    }

    fn activate_display_item(&mut self) {
        let popup = match self.display_cursor {
            0 => {
                self.toggle_mode();
                None
            }
            1 => {
                self.show_line_numbers = !self.show_line_numbers;
                None
            }
            2 => Some(ChoicePopup {
                kind: PopupKind::LineEnding,
                selected: selected_line_ending_index(self.line_ending),
            }),
            3 => {
                self.auto_wrap = !self.auto_wrap;
                None
            }
            4 => {
                self.display_format = match self.display_format {
                    DisplayFormat::Ascii => DisplayFormat::Hex,
                    DisplayFormat::Hex => DisplayFormat::Ascii,
                };
                None
            }
            5 => {
                self.send_format = match self.send_format {
                    SendFormat::Ascii => SendFormat::Hex,
                    SendFormat::Hex => SendFormat::Ascii,
                };
                None
            }
            _ => None,
        };
        self.choice_popup = popup;
    }

    fn handle_terminal_key(&mut self, key: KeyEvent) {
        let code = key.code;
        let modifiers = key.modifiers;
        match code {
            KeyCode::Up if modifiers.contains(KeyModifiers::CONTROL) => self.scroll_up(1),
            KeyCode::Down if modifiers.contains(KeyModifiers::CONTROL) => self.scroll_down(1),
            KeyCode::Char('k') if modifiers.contains(KeyModifiers::CONTROL) => self.scroll_up(1),
            KeyCode::Char('j') if modifiers.contains(KeyModifiers::CONTROL) => self.scroll_down(1),
            KeyCode::Enter => {
                let cmd = self.terminal_input.clone();
                self.terminal_input.clear();
                self.send_command(cmd);
            }
            KeyCode::Backspace => {
                self.terminal_input.pop();
            }
            KeyCode::Char(ch) => {
                self.terminal_input.push(ch);
            }
            _ => {}
        }
    }

    fn terminal_viewport_size(&self) -> (u16, u16) {
        let (width, height) = crossterm::terminal::size().unwrap_or((120, 40));
        let root = Rect::new(0, 0, width, height);
        let rows = Layout::default()
            .direction(Direction::Vertical)
            .constraints([Constraint::Min(5), Constraint::Length(2)])
            .split(root);
        let columns = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([Constraint::Percentage(20), Constraint::Percentage(80)])
            .split(rows[0]);
        let panel3_outer = if self.mode == InputMode::Terminal {
            columns[1]
        } else {
            Layout::default()
                .direction(Direction::Vertical)
                .constraints([Constraint::Percentage(75), Constraint::Percentage(25)])
                .split(columns[1])[0]
        };
        let panel3_inner = self
            .panel_block(
                PanelFocus::Terminal,
                format!("[3] {}", PanelFocus::Terminal.title()),
            )
            .inner(panel3_outer);
        (panel3_inner.width, panel3_inner.height)
    }

    fn current_scroll_lines(&self) -> Vec<String> {
        if self.mode == InputMode::Terminal {
            self.terminal_plain_lines()
        } else {
            self.logs.clone()
        }
    }

    fn scroll_up(&mut self, step: usize) {
        let (width, height) = self.terminal_viewport_size();
        let lines = self.current_scroll_lines();
        let max_scroll = self.max_scroll(&lines, width, height);
        let current = self.current_scroll(max_scroll);
        self.log_scroll_top = Some(current.saturating_sub(step));
    }

    fn scroll_down(&mut self, step: usize) {
        let (width, height) = self.terminal_viewport_size();
        let lines = self.current_scroll_lines();
        let max_scroll = self.max_scroll(&lines, width, height);
        let current = self.current_scroll(max_scroll);
        let next = (current + step).min(max_scroll);
        self.log_scroll_top = if next >= max_scroll { None } else { Some(next) };
    }

    fn reset_command_history_nav(&mut self) {
        self.command_history_index = None;
        self.command_history_draft = None;
    }

    fn push_command_history(&mut self, cmd: &str) {
        if cmd.is_empty() {
            return;
        }
        self.command_history.push(cmd.to_string());
        const MAX_COMMAND_HISTORY: usize = 20;
        if self.command_history.len() > MAX_COMMAND_HISTORY {
            let overflow = self.command_history.len() - MAX_COMMAND_HISTORY;
            self.command_history.drain(0..overflow);
        }
    }

    fn command_history_up(&mut self) {
        if self.command_history.is_empty() {
            return;
        }

        match self.command_history_index {
            None => {
                self.command_history_draft = Some(self.command_input.clone());
                let idx = self.command_history.len() - 1;
                self.command_history_index = Some(idx);
                self.command_input = self.command_history[idx].clone();
            }
            Some(idx) => {
                if idx > 0 {
                    let next = idx - 1;
                    self.command_history_index = Some(next);
                    self.command_input = self.command_history[next].clone();
                }
            }
        }
    }

    fn command_history_down(&mut self) {
        let Some(idx) = self.command_history_index else {
            return;
        };

        if idx + 1 < self.command_history.len() {
            let next = idx + 1;
            self.command_history_index = Some(next);
            self.command_input = self.command_history[next].clone();
            return;
        }

        self.command_history_index = None;
        self.command_input = self.command_history_draft.take().unwrap_or_default();
    }

    fn handle_command_key(&mut self, code: KeyCode) {
        match code {
            KeyCode::Esc => {
                self.mode = InputMode::Terminal;
                self.focus = PanelFocus::Terminal;
                self.command_input.clear();
                self.reset_command_history_nav();
            }
            KeyCode::Enter => {
                if self.command_input.is_empty() {
                    return;
                }

                let cmd = self.command_input.clone();
                self.push_command_history(&cmd);
                self.command_input.clear();
                self.reset_command_history_nav();
                self.send_command(cmd);
            }
            KeyCode::Up => self.command_history_up(),
            KeyCode::Down => self.command_history_down(),
            KeyCode::Backspace => {
                self.reset_command_history_nav();
                self.command_input.pop();
            }
            KeyCode::Char(ch) => {
                self.reset_command_history_nav();
                self.command_input.push(ch);
            }
            _ => {}
        }
    }

    fn popup_title(&self, kind: PopupKind) -> &'static str {
        match kind {
            PopupKind::Port => "Select Port",
            PopupKind::Baud => "Select Baud",
            PopupKind::DataBits => "Select Data Bits",
            PopupKind::StopBits => "Select Stop Bits",
            PopupKind::Parity => "Select Parity",
            PopupKind::LineEnding => "Select Line Ending",
        }
    }

    fn popup_options(&self, kind: PopupKind) -> Vec<String> {
        match kind {
            PopupKind::Port => {
                if self.ports.is_empty() {
                    vec!["<No port>".to_string()]
                } else {
                    self.ports.clone()
                }
            }
            PopupKind::Baud => {
                let mut out: Vec<String> =
                    self.baud_presets.iter().map(|b| b.to_string()).collect();
                let label = match self.custom_baud {
                    Some(v) => format!("Custom {v}"),
                    None => "Custom".to_string(),
                };
                out.push(label);
                out
            }
            PopupKind::DataBits => vec![
                "5".to_string(),
                "6".to_string(),
                "7".to_string(),
                "8".to_string(),
            ],
            PopupKind::StopBits => vec!["1".to_string(), "2".to_string()],
            PopupKind::Parity => vec!["None".to_string(), "Odd".to_string(), "Even".to_string()],
            PopupKind::LineEnding => vec!["LF".to_string(), "CR".to_string(), "CRLF".to_string()],
        }
    }

    fn apply_popup_selection(&mut self, kind: PopupKind, selected: usize) {
        match kind {
            PopupKind::Port => {
                if !self.ports.is_empty() {
                    self.selected_port = selected.min(self.ports.len() - 1);
                }
            }
            PopupKind::Baud => {
                if selected < self.baud_presets.len() {
                    self.selected_baud = self.baud_presets[selected];
                } else {
                    self.custom_baud_input = Some(self.selected_baud.to_string());
                }
            }
            PopupKind::DataBits => {
                self.selected_data_bits = match selected {
                    0 => DataBits::Five,
                    1 => DataBits::Six,
                    2 => DataBits::Seven,
                    _ => DataBits::Eight,
                };
            }
            PopupKind::StopBits => {
                self.selected_stop_bits = if selected == 0 {
                    StopBits::One
                } else {
                    StopBits::Two
                };
            }
            PopupKind::Parity => {
                self.selected_parity = match selected {
                    0 => Parity::None,
                    1 => Parity::Odd,
                    _ => Parity::Even,
                };
            }
            PopupKind::LineEnding => {
                self.line_ending = match selected {
                    0 => LineEnding::Lf,
                    1 => LineEnding::Cr,
                    _ => LineEnding::CrLf,
                };
            }
        }
    }

    fn toggle_connection(&mut self) {
        if self.connected {
            self.serial_conn = None;
            self.connected = false;
            self.push_local_log("Serial disconnected.");
            return;
        }

        let Some(port_name) = self.ports.get(self.selected_port).cloned() else {
            self.push_local_log("No available serial ports.");
            return;
        };

        let baud = self.selected_baud;
        match serialport::new(port_name.clone(), baud)
            .data_bits(self.selected_data_bits)
            .stop_bits(self.selected_stop_bits)
            .parity(self.selected_parity)
            .timeout(Duration::from_millis(20))
            .open()
        {
            Ok(mut port) => {
                let _ = port.write_data_terminal_ready(self.dtr_enabled);
                let _ = port.write_request_to_send(self.rts_enabled);
                self.attach_connection(port);
                self.connected = true;
                self.push_local_log(&format!("Connected: {port_name} @ {baud}"));
            }
            Err(e) => {
                self.push_local_log(&format!("Connect failed: {e}"));
            }
        }
    }

    fn attach_connection(&mut self, mut port: Box<dyn SerialPort>) {
        let tx_read = self.tx_serial_data.clone();
        let tx_err = self.tx_write_err.clone();
        let (tx_write, rx_write) = mpsc::channel::<Vec<u8>>();

        std::thread::spawn(move || {
            let mut read_buf = [0u8; 1024];
            loop {
                match port.read(&mut read_buf) {
                    Ok(n) if n > 0 => {
                        if tx_read.send(read_buf[..n].to_vec()).is_err() {
                            break;
                        }
                    }
                    Ok(_) => {}
                    Err(ref e) if e.kind() == std::io::ErrorKind::TimedOut => {}
                    Err(e) => {
                        let _ = tx_err.send(format!("Read error: {e}"));
                        break;
                    }
                }

                while let Ok(out) = rx_write.try_recv() {
                    if let Err(e) = port.write_all(&out) {
                        let _ = tx_err.send(format!("Write error: {e}"));
                        break;
                    }
                }
            }
        });

        self.serial_conn = Some(SerialConnection { tx: tx_write });
    }

    fn send_command(&mut self, cmd: String) {
        if self.mode == InputMode::Command {
            self.push_local_log(&format!("<- {cmd}"));
        } else {
            self.push_local_log(&cmd);
        }

        if let Some(conn) = &self.serial_conn {
            let mut bytes = match self.send_format {
                SendFormat::Ascii => cmd.into_bytes(),
                SendFormat::Hex => match parse_hex_bytes(&cmd) {
                    Ok(v) => v,
                    Err(e) => {
                        self.push_local_log(&format!("Invalid HEX input: {e}"));
                        return;
                    }
                },
            };
            bytes.extend_from_slice(match self.line_ending {
                LineEnding::Lf => b"\n",
                LineEnding::Cr => b"\r",
                LineEnding::CrLf => b"\r\n",
            });
            if let Err(e) = conn.tx.send(bytes) {
                self.push_local_log(&format!("Send failed: {e}"));
            }
        } else {
            self.push_local_log("Not connected. Command was not sent.");
        }
    }

    fn drain_serial_events(&mut self) {
        while let Ok(raw) = self.rx_serial_data.try_recv() {
            let content = self.format_serial_data(&raw);
            if self.mode == InputMode::Command {
                self.push_data_log(format!("-> {content}"));
            } else {
                self.push_data_log(content);
            }
        }

        while let Ok(err) = self.rx_write_err.try_recv() {
            self.push_local_log(&err);
            self.connected = false;
            self.serial_conn = None;
        }
    }

    fn format_serial_data(&self, raw: &[u8]) -> String {
        match self.display_format {
            DisplayFormat::Ascii => {
                let mut out = String::with_capacity(raw.len());
                for &b in raw {
                    if b.is_ascii_graphic() || b == b' ' || b == b'\n' || b == b'\r' || b == b'\t' {
                        out.push(b as char);
                    } else {
                        out.push('.');
                    }
                }
                out.replace('\r', "")
            }
            DisplayFormat::Hex => raw
                .iter()
                .map(|b| format!("{b:02X}"))
                .collect::<Vec<_>>()
                .join(" "),
        }
    }

    fn push_log(&mut self, mut line: String) {
        if self.show_line_numbers {
            line = format!("{} | {line}", self.next_line_no);
            self.next_line_no += 1;
        }
        self.logs.push(line);
        self.trim_logs();
    }

    fn push_data_log(&mut self, line: String) {
        self.push_log(line);
    }

    fn push_local_log(&mut self, text: &str) {
        self.push_log(text.to_string());
    }

    fn trim_logs(&mut self) {
        const MAX_LOG_LINES: usize = 5000;
        if self.logs.len() > MAX_LOG_LINES {
            let overflow = self.logs.len() - MAX_LOG_LINES;
            self.logs.drain(0..overflow);
        }
    }
}

fn centered_rect(percent_x: u16, percent_y: u16, area: Rect) -> Rect {
    let vertical = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Percentage((100 - percent_y) / 2),
            Constraint::Percentage(percent_y),
            Constraint::Percentage((100 - percent_y) / 2),
        ])
        .split(area);

    Layout::default()
        .direction(Direction::Horizontal)
        .constraints([
            Constraint::Percentage((100 - percent_x) / 2),
            Constraint::Percentage(percent_x),
            Constraint::Percentage((100 - percent_x) / 2),
        ])
        .split(vertical[1])[1]
}

fn collect_port_names() -> Vec<String> {
    serialport::available_ports()
        .map(|ports| ports.into_iter().map(|p| p.port_name).collect())
        .unwrap_or_default()
}

fn yes_no(flag: bool) -> &'static str {
    if flag { "On" } else { "Off" }
}

fn line_ending_label(v: LineEnding) -> &'static str {
    match v {
        LineEnding::Lf => "LF",
        LineEnding::Cr => "CR",
        LineEnding::CrLf => "CRLF",
    }
}

fn selected_line_ending_index(v: LineEnding) -> usize {
    match v {
        LineEnding::Lf => 0,
        LineEnding::Cr => 1,
        LineEnding::CrLf => 2,
    }
}

fn data_bits_label(v: DataBits) -> &'static str {
    match v {
        DataBits::Five => "5",
        DataBits::Six => "6",
        DataBits::Seven => "7",
        DataBits::Eight => "8",
    }
}

fn selected_data_bits_index(v: DataBits) -> usize {
    match v {
        DataBits::Five => 0,
        DataBits::Six => 1,
        DataBits::Seven => 2,
        DataBits::Eight => 3,
    }
}

fn stop_bits_label(v: StopBits) -> &'static str {
    match v {
        StopBits::One => "1",
        StopBits::Two => "2",
    }
}

fn selected_stop_bits_index(v: StopBits) -> usize {
    match v {
        StopBits::One => 0,
        StopBits::Two => 1,
    }
}

fn parity_label(v: Parity) -> &'static str {
    match v {
        Parity::None => "None",
        Parity::Odd => "Odd",
        Parity::Even => "Even",
    }
}

fn selected_parity_index(v: Parity) -> usize {
    match v {
        Parity::None => 0,
        Parity::Odd => 1,
        Parity::Even => 2,
    }
}

fn keycap(text: &'static str) -> Span<'static> {
    Span::styled(
        format!("[{text}]"),
        Style::default().fg(COLOR_HINT).add_modifier(Modifier::BOLD),
    )
}

fn hint(text: &'static str) -> Span<'static> {
    Span::styled(format!(" {text} "), Style::default().fg(COLOR_FG))
}

fn separator() -> Span<'static> {
    Span::styled(" | ", Style::default().fg(COLOR_BORDER_INACTIVE))
}

fn rect_contains(rect: Rect, x: u16, y: u16) -> bool {
    x >= rect.x
        && x < rect.x.saturating_add(rect.width)
        && y >= rect.y
        && y < rect.y.saturating_add(rect.height)
}

fn parse_hex_bytes(input: &str) -> Result<Vec<u8>, &'static str> {
    let compact: String = input.chars().filter(|c| !c.is_ascii_whitespace()).collect();
    if compact.is_empty() {
        return Ok(Vec::new());
    }
    if !compact.len().is_multiple_of(2) {
        return Err("hex length must be even");
    }

    let mut out = Vec::with_capacity(compact.len() / 2);
    let bytes = compact.as_bytes();
    let mut i = 0usize;
    while i < bytes.len() {
        let hi = bytes[i] as char;
        let lo = bytes[i + 1] as char;
        let pair = [hi, lo].iter().collect::<String>();
        let value = u8::from_str_radix(&pair, 16).map_err(|_| "contains non-hex character")?;
        out.push(value);
        i += 2;
    }
    Ok(out)
}

fn main() -> std::io::Result<()> {
    execute!(
        std::io::stdout(),
        Show,
        EnableBlinking,
        SetCursorStyle::BlinkingBar,
        EnableMouseCapture
    )?;
    let mut terminal = ratatui::init();
    let mut app = App::new();
    let run_result = app.run(&mut terminal);

    execute!(std::io::stdout(), DisableMouseCapture)?;
    ratatui::restore();

    run_result
}
