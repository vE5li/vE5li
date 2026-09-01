--   ___  __   __ _  ____  ____  _  _  ____
--  / __)/  \ (  ( \(_  _)(  __)( \/ )(_  _)
-- ( (__(  O )/    /  )(   ) _)  )  (   )(
--  \___)\__/ \_)__) (__) (____)(_/\_) (__)
--
structure Context where
  indentation : Nat
deriving Repr

def Context.indent (self : Context) : String := String.ofList (List.replicate self.indentation ' ')
def Context.step (self : Context) : Context := { self with indentation := self.indentation + 2 }

--  __ _   __  ____  ____
-- (  ( \ /  \(    \(  __)
-- /    /(  O )) D ( ) _)
-- \_)__) \__/(____/(____)
--
structure ProjectInfo where
  name : String
  description : String
  url : String := "https://github.com/vE5li/" ++ name
deriving Repr

structure SectionInfo where
  text : String
deriving Repr

inductive Node where
  | project : ProjectInfo -> List Node -> Node
  | section : SectionInfo -> List Node -> Node
deriving Repr

def Node.Δ (name description : String) (children : List Node := []) : Node :=
  .project { name, description } children

def Node.π (text : String) (children : List Node) : Node :=
  .section { text } children

def Node.markdownify (node : Node) (context : Context) (acc : String) : String :=
  match node with
  | .project info children =>
    let acc := acc ++ context.indent ++ s!"- [**{info.name}**]({info.url}) - {info.description}\n"
    children.foldl (fun acc child => child.markdownify context.step acc) acc
  | .section info children =>
    let acc := acc ++ context.indent ++ s!"- *{info.text}:*\n"
    children.foldl (fun acc child => child.markdownify context.step acc) acc

open Node (Δ π)

--   __   ____  ____   __
--  / _\ (  _ \(  __) / _\
-- /    \ )   / ) _) /    \
-- \_/\_/(__\_)(____)\_/\_/
--

-- Stride: 260
-- Width: 9
-- Height: 7
def font : String := r#"
    ___       ___       ___       ___       ___       ___       ___       ___       ___       ___       ___       ___       ___       ___       ___       ___       ___       ___       ___       ___       ___       ___       ___       ___       ___       ___   
   /\  \     /\  \     /\  \     /\  \     /\  \     /\  \     /\  \     /\__\     /\  \     /\  \     /\__\     /\__\     /\__\     /\__\     /\  \     /\  \     /\  \     /\  \     /\  \     /\  \     /\__\     /\__\     /\__\     /\__\     /\__\     /\  \  
  /::\  \   /::\  \   /::\  \   /::\  \   /::\  \   /::\  \   /::\  \   /:/__/_   _\:\  \   _\:\  \   /:/ _/_   /:/  /    /::L_L_   /:| _|_   /::\  \   /::\  \   /::\  \   /::\  \   /::\  \    \:\  \   /:/ _/_   /:/ _/_   /:/\__\   |::L__L   |::L__L   _\:\  \ 
 /::\:\__\ /::\:\__\ /:/\:\__\ /:/\:\__\ /::\:\__\ /::\:\__\ /:/\:\__\ /::\/\__\ /\/::\__\ /\/::\__\ /::-"\__\ /:/__/    /:/L:\__\ /::|/\__\ /:/\:\__\ /::\:\__\  \:\:\__\ /::\:\__\ /\:\:\__\   /::\__\ /:/_/\__\ |::L/\__\ /:/:/\__\ /::::\__\  |:::\__\ /::::\__\
 \/\::/  / \:\::/  / \:\ \/__/ \:\/:/  / \:\:\/  / \/\:\/__/ \:\:\/__/ \/\::/  / \::/\/__/ \::/\/__/ \;:;-",-" \:\  \    \/_/:/  / \/|::/  / \:\/:/  / \/\::/  /   \::/  / \;:::/  / \:\:\/__/  /:/\/__/ \:\/:/  / |::::/  / \::/:/  / \;::;/__/  /:;;/__/ \::;;/__/
   /:/  /   \::/  /   \:\__\    \::/  /   \:\/  /     \/__/   \::/  /    /:/  /   \:\__\    \/__/     |:|  |    \:\__\     /:/  /    |:/  /   \::/  /     \/__/    /:/  /   |:\/__/   \::/  /   \/__/     \::/  /   L;;/__/   \::/  /   |::|__|   \/__/     \:\__\  
   \/__/     \/__/     \/__/     \/__/     \/__/               \/__/     \/__/     \/__/               \|__|     \/__/     \/__/     \/__/     \/__/               \/__/     \|__|     \/__/               \/__/               \/__/     \/__/               \/__/
"#

inductive ColorType where
  | none
  | inner
  | border
  deriving Repr, BEq

def startPosition (line : Nat) (character : Char) : String.Pos.Raw := String.Pos.Raw.mk (((character.toNat - 'a'.toNat) * 10) + (261 * line) + 1)

def endPosition (line : Nat) (character : Char) : String.Pos.Raw := String.Pos.Raw.mk (((character.toNat - 'a'.toNat + 1) * 10) + (261 * line) + 1)

def charToArt (line : Nat) (character : Char) : String := font.extract (font.pos! (startPosition line character)) (font.pos! (endPosition line character))

def toColorType : Char -> ColorType
  | ' ' => ColorType.none
  | ':' => ColorType.inner
  | _ => ColorType.border

def fooColorType (foo: String) : ColorType :=
  if foo.isEmpty then ColorType.none else toColorType foo.back

def styleForType : Nat -> ColorType -> String
  | _, ColorType.none   => r"color:#FFFFFF"
  | 2, ColorType.inner  => r"color:#FF0000;font-weight:bold"
  | 3, ColorType.inner  => r"color:#FF5500;font-weight:bold"
  | 4, ColorType.inner  => r"color:#FFAA00;font-weight:bold"
  | 5, ColorType.inner  => r"color:#FFFF00;font-weight:bold"
  | _, ColorType.inner  => r"color:#FF00B6;font-weight:bold"
  | _, ColorType.border => r"color:#00FFE5"

def fooFold (line : Nat) (acc : String) (character : Char) : String := if fooColorType acc == toColorType character then
  acc ++ character.toString else
  acc ++ s!"</span><span style=\"{styleForType line (toColorType character)}\">" ++ character.toString

def colorizeLine (line : Nat) (s : String) : String := s.toList.foldl (fooFold line) ""

def makeAsciiLine (line : Nat) (text: String) : String := s!"\n# <span>" ++ colorizeLine line (String.join (text.toLower.toList.map (charToArt line))) ++ "</span>"

def makeAsciiHeader (text : String) : String := s!"<pre style=\"font-family:'Courier New',Courier,monospace;font-size:8px;line-height:1.17;white-space:pre;color:#fff;padding:8px;margin:0;\">#{(makeAsciiLine 0 text)}{(makeAsciiLine 1 text)}{(makeAsciiLine 2 text)}{(makeAsciiLine 3 text)}{(makeAsciiLine 4 text)}{(makeAsciiLine 5 text)}{(makeAsciiLine 6 text)}\n#</pre>\n"

structure Area where
  name : String
  projects : List Node
deriving Repr

def Area.markdownify (area : Area) (context : Context) (acc : String) : String :=
  (area.projects.foldl (fun acc node => node.markdownify context acc) (acc ++ makeAsciiHeader area.name)) ++ "\n\n"

--    ___       ___       ___       ___       ___       ___       ___       ___       ___       ___       ___       ___       ___       ___
--   /\  \     /\__\     /\  \     /\  \     /\  \     /\  \     /\  \     /\  \     /\__\     /\  \     /\  \     /\__\     /\  \     /\  \
--  _\:\  \   /:| _|_   /::\  \   /::\  \   /::\  \   /::\  \    \:\  \   /::\  \   /:/ _/_   /::\  \    \:\  \   /:/ _/_   /::\  \   /::\  \
-- /\/::\__\ /::|/\__\ /::\:\__\ /::\:\__\ /::\:\__\ /\:\:\__\   /::\__\ /::\:\__\ /:/_/\__\ /:/\:\__\   /::\__\ /:/_/\__\ /::\:\__\ /::\:\__\
-- \::/\/__/ \/|::/  / \/\:\/__/ \;:::/  / \/\::/  / \:\:\/__/  /:/\/__/ \;:::/  / \:\/:/  / \:\ \/__/  /:/\/__/ \:\/:/  / \;:::/  / \:\:\/  /
--  \:\__\     |:/  /     \/__/   |:\/__/    /:/  /   \::/  /   \/__/     |:\/__/   \::/  /   \:\__\    \/__/     \::/  /   |:\/__/   \:\/  /
--   \/__/     \/__/               \|__|     \/__/     \/__/               \|__|     \/__/     \/__/               \/__/     \|__|     \/__/
--
def infrastructure := Area.mk "Infrastructure" [
  Δ "infrastructure" "My home infrastructure, including device configuration" [
    π "NixOS configuration" [
      Δ "lan-pam" "PAM for logging in to my machines using trusted devices on my network (such as my phone)",
      Δ "tagsy" "A file-sync client, tag based cloud storage, and shared clipboard",
      Δ "cross-cursor" "A single cross-shaped cursor for my graphical environment",
      Δ "anime-foot" "Fork of [foot](https://codeberg.org/dnkl/foot) with an anime girl in the background. That's it",
    ],
    π "Neovim configuration" [
      Δ "unified-text-objects.nvim" "Plugin that offers more consistent and powerful text objects",
      Δ "indent-object-provider.nvim" "Indentation based text objects for [unified-text-objects](https://github.com/vE5li/unified-text-objects.nvim)",
      Δ "native-object-provider.nvim" "Mostly Neovims native text objects but for [unified-text-objects](https://github.com/vE5li/unified-text-objects.nvim)",
      Δ "case-object-provider.nvim" "Word segment based text objects for [unified-text-objects](https://github.com/vE5li/unified-text-objects.nvim)",
      Δ "delete-assassin.nvim" "Plugin for keeping the cursor position fixed with delete operators",
      Δ "find-mode.nvim" "Plugin to quickly navigate [oil.nvim](https://github.com/stevearc/oil.nvim) buffers",
      Δ "better-goto-file.nvim" "Plugin to improve Neovims builtin `gf` and `gF` commands",
      Δ "bookmarks.nvim" "Plugin to provide bookmarks for navigating between different buffers and snippets",
      Δ "yop.nvim" "Fork of [yop.nvim](https://github.com/zdcthomas/yop.nvim) that fixes some issues of the original repository",
      Δ "cmp-buffer" "Fork of [cmp-buffer](https://github.com/hrsh7th/cmp-buffer) that adds suggestions in different casing (now archived)",
    ],
  ]
]

--    ___       ___       ___       ___       ___       ___       ___       ___            ___       ___       ___       ___       ___       ___
--   /\  \     /\  \     /\  \     /\__\     /\  \     /\  \     /\  \     /\__\          /\  \     /\__\     /\__\     /\  \     /\__\     /\  \
--  /::\  \   /::\  \   /::\  \   /:| _|_   /::\  \   /::\  \   /::\  \   /:/ _/_        /::\  \   /:| _|_   /:/  /    _\:\  \   /:| _|_   /::\  \
-- /::\:\__\ /::\:\__\ /:/\:\__\ /::|/\__\ /::\:\__\ /::\:\__\ /:/\:\__\ /::-"\__\      /:/\:\__\ /::|/\__\ /:/__/    /\/::\__\ /::|/\__\ /::\:\__\
-- \;:::/  / \/\::/  / \:\:\/__/ \/|::/  / \/\::/  / \;:::/  / \:\/:/  / \;:;-",-"      \:\/:/  / \/|::/  / \:\  \    \::/\/__/ \/|::/  / \:\:\/  /
--  |:\/__/    /:/  /   \::/  /    |:/  /    /:/  /   |:\/__/   \::/  /   |:|  |         \::/  /    |:/  /   \:\__\    \:\__\     |:/  /   \:\/  /
--   \|__|     \/__/     \/__/     \/__/     \/__/     \|__|     \/__/     \|__|          \/__/     \/__/     \/__/     \/__/     \/__/     \/__/
--
def ragnarok_online := Area.mk "Ragnarok Online" [
  Δ "korangar" "A next-gen Ragnarok Online client written in Rust" [
    Δ "lunify" "Rust crate to convert Lua files to a format that Korangar can work with",
    Δ "rust-state" "Rust crate to provide state management for Korangar's user interface",
    Δ "korangar-rathena" "NixOS setup for Korangar development servers"
  ]
]

--    ___       ___       ___       ___       ___       ___       ___       ___       ___
--   /\__\     /\  \     /\__\     /\  \     /\  \     /\  \     /\  \     /\  \     /\  \
--  /:/ _/_   /::\  \   |::L__L   /::\  \   /::\  \   /::\  \   /::\  \   /::\  \   /::\  \
-- /::-"\__\ /::\:\__\  |:::\__\ /::\:\__\ /:/\:\__\ /::\:\__\ /::\:\__\ /:/\:\__\ /\:\:\__\
-- \;:;-",-" \:\:\/  /  /:;;/__/ \:\::/  / \:\/:/  / \/\::/  / \;:::/  / \:\/:/  / \:\:\/__/
--  |:|  |    \:\/  /   \/__/     \::/  /   \::/  /    /:/  /   |:\/__/   \::/  /   \::/  /
--   \|__|     \/__/               \/__/     \/__/     \/__/     \|__|     \/__/     \/__/
--
def keyboards := Area.mk "Keyboards" [
  Δ "iamb-manuform" "The Dactyl Manuform I use as a daily driver",
  Δ "butterware" "Attempt at a Bluetooth capable keyboard firmware written in Rust",
  Δ "PCB_for_HandwiredKeyboards" "Fork of [PCB_for_HandwiredKeyboards](https://github.com/PitBarber/PCB_for_HandwiredKeyboards) that uses more convenient LEDs",
]

--    ___       ___       ___       ___       ___       ___       ___       ___       ___       ___       ___       ___       ___
--   /\__\     /\  \     /\  \     /\  \     /\  \     /\__\     /\__\     /\  \     /\__\     /\  \     /\  \     /\__\     /\  \
--  /::L_L_   _\:\  \   /::\  \   /::\  \   /::\  \   /:/  /    /:/  /    /::\  \   /:| _|_   /::\  \   /::\  \   /:/ _/_   /::\  \
-- /:/L:\__\ /\/::\__\ /\:\:\__\ /:/\:\__\ /::\:\__\ /:/__/    /:/__/    /::\:\__\ /::|/\__\ /::\:\__\ /:/\:\__\ /:/_/\__\ /\:\:\__\
-- \/_/:/  / \::/\/__/ \:\:\/__/ \:\ \/__/ \:\:\/  / \:\  \    \:\  \    \/\::/  / \/|::/  / \:\:\/  / \:\/:/  / \:\/:/  / \:\:\/__/
--   /:/  /   \:\__\    \::/  /   \:\__\    \:\/  /   \:\__\    \:\__\     /:/  /    |:/  /   \:\/  /   \::/  /   \::/  /   \::/  /
--   \/__/     \/__/     \/__/     \/__/     \/__/     \/__/     \/__/     \/__/     \/__/     \/__/     \/__/     \/__/     \/__/
--
def miscellaneous := Area.mk "Miscellaneous" [
  Δ "wallpapers" "Just some wallpapers I made",
  Δ "vE5li.github.io" "Rusty wallpaper generator used for one of my [wallpapers](https://github.com/vE5li/wallpapers)",
  Δ "hydrox-kernel" "Very old fun project to write an AArch64 kernel for the Raspberry Pi",
]

def areas : List Area := [infrastructure, ragnarok_online, keyboards, miscellaneous]

def main := IO.print ((String.toFormat (areas.foldl (fun acc area => area.markdownify (Context.mk 0) acc) "")) ++ "This file was generated by main.lean")
