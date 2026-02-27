#!/bin/bash
# Desktop notification when Claude needs attention
# Linux/WSL: uses notify-send if available, falls back to echo
INPUT=$(cat)
MESSAGE=$(echo "$INPUT" | jq -r '.message // "Claude needs attention"')
TITLE=$(echo "$INPUT" | jq -r '.title // "Claude Code"')

# Try notify-send (Linux desktop)
if command -v notify-send &>/dev/null; then
  notify-send "$TITLE" "$MESSAGE" 2>/dev/null
  exit 0
fi

# Try Windows toast via PowerShell (WSL)
if command -v powershell.exe &>/dev/null; then
  powershell.exe -Command "
    [Windows.UI.Notifications.ToastNotificationManager, Windows.UI.Notifications, ContentType = WindowsRuntime] | Out-Null
    \$template = [Windows.UI.Notifications.ToastNotificationManager]::GetTemplateContent([Windows.UI.Notifications.ToastTemplateType]::ToastText02)
    \$template.GetElementsByTagName('text')[0].AppendChild(\$template.CreateTextNode('$TITLE')) | Out-Null
    \$template.GetElementsByTagName('text')[1].AppendChild(\$template.CreateTextNode('$MESSAGE')) | Out-Null
    \$toast = [Windows.UI.Notifications.ToastNotification]::new(\$template)
    [Windows.UI.Notifications.ToastNotificationManager]::CreateToastNotifier('Claude Code').Show(\$toast)
  " 2>/dev/null
fi

exit 0
