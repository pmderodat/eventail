with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with GNAT.OS_Lib;
with GNAT.Strings; use GNAT.Strings;

with GNATCOLL.Utils; use GNATCOLL.Utils;

package body HTML is

   LF : constant Character := Character'Val (10);

   --  TODO??? Add HTML entities escaping where needed

   function HRef (H : Handle; File : String) return String;
   procedure Escape (Text : in out GNAT.Strings.String_Access);
   function Locate (Program_Name : String) return String;
   procedure Free (Args : in out GNAT.OS_Lib.Argument_List);

   ----------
   -- HRef --
   ----------

   function HRef (H : Handle; File : String) return String is
      File_Path : constant Virtual_File := Create_From_Dir (H.Root, +File);
   begin
      return +Relative_Path (File_Path, Create (Dir_Name (H.File_Path)));
   end HRef;

   ------------
   -- Escape --
   ------------

   procedure Escape (Text : in out GNAT.Strings.String_Access) is
      Result : Unbounded_String;
   begin
      for C of Text.all loop
         case C is
            when '<' => Append (Result, "&lt;");
            when '>' => Append (Result, "&gt;");
            when '&' => Append (Result, "&amp;");
            when others => Append (Result, C);
         end case;
      end loop;
      Free (Text);
      Text := new String'(To_String (Result));
   end Escape;

   -----------
   -- Start --
   -----------

   function Start
     (File  : Virtual_File;
      Root  : Virtual_File;
      Title : String) return Handle
   is
      H : Handle := (Write_File (File), File, Root);
   begin
      Write (H.File, "<html>" & LF);
      Write (H.File, "<head>" & LF);
      Write
        (H.File,
         "<link rel=""stylesheet"" href="""
         & HRef (H, "main.css") & """ />" & LF);
      Write
        (H.File,
         "<link rel=""stylesheet"" href="""
         & HRef (H, "code.css") & """ />" & LF);
      Write (H.File, "<title>" & Title & "</title>" & LF);
      Write (H.File, "</head>" & LF);
      Write (H.File, "<body>" & LF);
      Write (H.File, "<h1>" & Title & "</h1>" & LF);
      return H;
   end Start;

   ----------
   -- Stop --
   ----------

   procedure Stop (H : in out Handle) is
   begin
      Write (H.File, "</body>" & LF);
      Write (H.File, "</html>" & LF);
      Close (H.File);
   end Stop;

   ------------------
   -- Add_Backlink --
   ------------------

   procedure Add_Backlink (H : in out Handle; Name, HRef : String) is
   begin
      Write
        (H.File,
         "<p><a class=""nav"" href=""" & HRef & """>Back to "
         & Name & "</a></p>" & LF);
   end Add_Backlink;

   ----------------
   -- Start_List --
   ----------------

   procedure Start_List (H : in out Handle) is
   begin
      Write (H.File, "<ul>" & LF);
   end Start_List;

   ---------------
   -- Stop_List --
   ---------------

   procedure Stop_List (H : in out Handle) is
   begin
      Write (H.File, "</ul>" & LF);
   end Stop_List;

   --------------
   -- Add_Item --
   --------------

   procedure Add_Item (H : in out Handle; Name, HRef : String) is
   begin
      Write
        (H.File,
         "<li><a class=""nav"" href=""" & HRef & """>"
         & Name & "</a></li>" & LF);
   end Add_Item;

   --------------
   -- Add_Code --
   --------------

   procedure Add_Code
     (H           : in out Handle;
      Source_File : Virtual_File;
      Language    : GNATCOLL.Symbols.Symbol)
   is
      Pygments_File : constant Virtual_File := Create_From_Dir
        (Get_Tmp_Directory, +("eventail-" & (+Base_Name (Source_File))));

      Lang          : constant String := GNATCOLL.Symbols.Get (Language).all;
      Args          : GNAT.OS_Lib.Argument_List :=
        (new String'("-l"), new String'(Lang),
         new String'("-f"), new String'("html"),
         new String'("-O"), new String'("nowrap=True"),
         new String'("-o"), new String'(+Full_Name (Pygments_File)),
         new String'(+Full_Name (Source_File)));

      Success       : Boolean;
      Content       : GNAT.Strings.String_Access;

   begin
      GNAT.OS_Lib.Spawn (Locate ("pygmentize"), Args, Success);
      if Success then
         Content := Read_File (Pygments_File);
      else
         Put_Line ("Error: pygmentize failed");
         Content := Read_File (Source_File);
         Escape (Content);
      end if;

      --  TODO??? generate appropriate warning/error on failure

      if Content = null then
         Write (H.File, "<p>Could not read the source code file.</p>" & LF);
      else
         Write (H.File, "<pre><code>");

         declare
            Lines       : constant Unbounded_String_Array :=
               Split (Content.all, ASCII.LF, Omit_Empty_Lines => False);
            Last_Number : constant String := Image (Lines'Length, 0);
            Width       : constant Integer := Last_Number'Length;
         begin
            Free (Content);

            for I in Lines'Range loop
               --  Prepend line numbers

               Write
                 (H.File,
                  "<span class=""lineno"">"
                  & Image (I - Lines'First + 1, Width, Padding => ' ')
                  & "</span>  ");
               Write (H.File, To_String (Lines (I)) & ASCII.LF);
            end loop;
         end;

         Write (H.File, "</code></pre>" & LF);
      end if;

      Delete (Pygments_File, Success);
      pragma Unreferenced (Success);

      Free (Args);
   end Add_Code;

   -----------------
   -- Setup_Media --
   -----------------

   procedure Setup_Media is
      Main_Style  : Writable_File := Write_File (Create (+"main.css"));
      Success     : Boolean;
      Return_Code : Integer;

      Args        : GNAT.OS_Lib.Argument_List :=
        (new String'("-S"),
         new String'("monokai"),
         new String'("-f"),
         new String'("html"));

   begin
      Write (Main_Style, "html {" & LF);
      Write (Main_Style, "    background: #181818;" & LF);
      Write (Main_Style, "    color: #a0a0a0;" & LF);
      Write (Main_Style, "}" & LF);
      Write (Main_Style, "a {" & LF);
      Write (Main_Style, "    color: inherit;" & LF);
      Write (Main_Style, "    text-decoration: none;" & LF);
      Write (Main_Style, "}" & LF);
      Write (Main_Style, "a:hover {" & LF);
      Write (Main_Style, "    text-decoration: underline;" & LF);
      Write (Main_Style, "}" & LF);
      Write (Main_Style, "a.nav {" & LF);
      Write (Main_Style, "    color: #808080;" & LF);
      Write (Main_Style, "    font-weight: bold;" & LF);
      Write (Main_Style, "}" & LF);
      Close (Main_Style);

      --  TODO??? generate appropriate warning/error on failure

      GNAT.OS_Lib.Spawn
        (Program_Name => Locate ("pygmentize"),
         Args         => Args,
         Output_File  => "code.css",
         Success      => Success,
         Return_Code  => Return_Code);
      if not Success or else Return_Code /= 0 then
         Put_Line
           ("Error: pygmentize failed: return code = "
            & Integer'Image (Return_Code));
      end if;

      Free (Args);
   end Setup_Media;

   ------------
   -- Locate --
   ------------

   function Locate (Program_Name : String) return String is
      Path : GNAT.Strings.String_Access :=
         GNAT.OS_Lib.Locate_Exec_On_Path (Program_Name);
      Result : constant String := Path.all;
   begin
      Free (Path);
      return Result;
   end Locate;

   ----------
   -- Free --
   ----------

   procedure Free (Args : in out GNAT.OS_Lib.Argument_List) is
      procedure Free is new Ada.Unchecked_Deallocation
        (String, GNAT.OS_Lib.String_Access);
   begin
      for Arg of Args loop
         Free (Arg);
      end loop;
   end Free;

end HTML;
