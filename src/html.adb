with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with GNAT.OS_Lib;
with GNAT.Strings; use GNAT.Strings;

with GNATCOLL.Symbols;
with GNATCOLL.Utils; use GNATCOLL.Utils;

package body HTML is

   LF : constant Character := Character'Val (10);

   --  TODO??? Add HTML entities escaping where needed

   function HRef (H : Handle; File : String) return String;
   procedure Escape (Text : in out GNAT.Strings.String_Access);
   function Locate (Program_Name : String) return String;
   function Get_Column
     (S : Unbounded_String;
      Column : Positive) return Natural;
   procedure Free (Args : in out GNAT.OS_Lib.Argument_List);

   procedure Insert_XRefs
     (Source_File  : Virtual_File;
      Library_Info : Library_Files.Library_Info_Type;
      Lines        : in out Unbounded_String_Array);
   --  Insert hyperlinks for xrefs in Lines. Lines must be the content of
   --  Source_File.

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

   ----------------
   -- Get_Column --
   ----------------

   function Get_Column
     (S : Unbounded_String;
      Column : Positive) return Natural
   is
      Result        : Positive := 1;
      Logical_Col   : Positive := 1;
      Next          : Character;
   begin
      loop
         --  If we meet some HTML tag or some HTML entity, skip it: it does not
         --  increment the column number.

         Next := Element (S, Result);
         while Next = '<' or else Next = '&' loop
            if Next = '<' then
               Next := '>';
            else
               Next := ';';
               Logical_Col := Logical_Col + 1;
            end if;

            while Element (S, Result) /= Next loop
               Result := Result + 1;
            end loop;
            Result := Result + 1;
            Next := Element (S, Result);
         end loop;

         if Logical_Col = Column then
            return Result;
         end if;

         Result := Result + 1;
         Logical_Col := Logical_Col + 1;
      end loop;
   end Get_Column;

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
     (H            : in out Handle;
      Source_File  : Virtual_File;
      Source_Info  : Repositories.Source_File_Type;
      Library_Info : Library_Files.Library_Info_Type)
   is
      Pygments_File : constant Virtual_File := Create_From_Dir
        (Get_Tmp_Directory, +("eventail-" & (+Base_Name (Source_File))));

      Lang          : constant String :=
         GNATCOLL.Symbols.Get (Source_Info.Language).all;
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
            Lines       : Unbounded_String_Array :=
               Split (Content.all, ASCII.LF, Omit_Empty_Lines => False);
            Last_Number : constant String := Image (Lines'Length, 0);
            Width       : constant Integer := Last_Number'Length;
         begin
            Free (Content);

            Insert_XRefs (Source_File, Library_Info, Lines);

            for I in Lines'Range loop
               --  Prepend line numbers

               Write
                 (H.File,
                  "<span id=""line" & Image (I - Lines'First + 1, 0)
                  & """ class=""lineno"">"
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

   ------------------
   -- Insert_XRefs --
   ------------------

   procedure Insert_XRefs
     (Source_File  : Virtual_File;
      Library_Info : Library_Files.Library_Info_Type;
      Lines        : in out Unbounded_String_Array)
   is
      use Repositories;
      use Repositories.Source_File_Maps;
      use Library_Files;
      use Library_Files.File_Table_Vectors;

      Src_Idx : Source_File_Index := No_Source_File;
   begin
      --  If there is no xref, there is nothing to do

      if Library_Info.File_Table.Is_Empty then
         return;
      end if;
      --
      --  Otherwise, find the index of Source_File in the file table

      for Cur in Library_Info.File_Table.Iterate loop
         if Element (Cur) /= Source_File_Maps.No_Element
               and then
            Key (Element (Cur)) = Source_File
         then
            Src_Idx := To_Index (Cur);
            exit;
         end if;
      end loop;

      --  A source file *must* appear in the dependency lines of its own
      --  Library File.

      pragma Assert (Src_Idx /= No_Source_File);

      --  Then, insert xref hyperlinks, only for usages that come from
      --  Source_File.

      for Usage of Library_Info.Usages loop
         if Usage.Sloc.File = Src_Idx then
            declare
               Def          : constant Definition_Type :=
                  Library_Info.Definitions.Element (Usage.Entity);
               Def_File     : constant Source_File_Maps.Cursor :=
                  Library_Info.File_Table.Element (Def.Sloc.File);
               Def_Filename : constant String := +Base_Name (Key (Def_File));
               Line         : Unbounded_String renames Lines (Usage.Sloc.Line);
               Id_First, Id_Last : Positive;
            begin
               Id_First := Get_Column (Line, Usage.Sloc.Column);
               Id_Last := Id_First + GNATCOLL.Symbols.Get (Def.Name)'Length;
               Insert (Line, Id_Last, "</a>");
               if Element (Def_File).Displayed then
                  Insert
                    (Line, Id_First,
                     "<a href=""../"
                     & Name (Element (Def_File).Project_Group.all)
                     & "/" & Def_Filename & ".html#line"
                     & Image (Def.Sloc.Line, 0) & """>");
               else
                  Insert
                    (Line, Id_First,
                     "<a title=""In " & Def_Filename & ", line"
                     & Positive'Image (Def.Sloc.Line) & """>");
               end if;
            end;
         end if;
      end loop;
   end Insert_XRefs;

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
