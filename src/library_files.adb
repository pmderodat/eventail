with Ada.Strings.Unbounded;

with GNAT.Regpat;  use GNAT.Regpat;
with GNAT.Strings; use GNAT.Strings;

with GNATCOLL.Symbols; use GNATCOLL.Symbols;
with GNATCOLL.Utils;    use GNATCOLL.Utils;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

package body Library_Files is

   Entity_Names : constant Symbol_Table_Access := Allocate;

   D_Regexp    : constant Pattern_Matcher :=
      Compile ("(([^""\t ]+)|(""([^""]|"""")+""))[\t ]");

   X_Regexp    : constant Pattern_Matcher :=
      Compile ("(\d+)");
   Decl_Regexp : constant Pattern_Matcher :=
      Compile
        ("^(\d+)([^0-9])(\d+)([^0-9])([a-zA-Z_0-9]+)"
         & "([<{][^>}]+[>}])*( |$)");
   Use_Regexp  : constant Pattern_Matcher :=
      Compile ("^(\d+\|)?(\d+)([^0-9])(\d+)");

   XRef_Allow : constant array (Character) of Boolean :=
     ('e' | 't'
      | '=' | '<' | '>' | '^' => False,
      others => True);

   function Get (Line : String_Access; Loc : Match_Location) return String;
   --  Return the substring in Line referenced by Loc, or the empty string if
   --  Loc is No_Match.

   function Unquote (Filename : String) return String;
   --  If needed, unquote a filename, such as the ones that can be found on D
   --  lines.

   ---------
   -- Get --
   ---------

   function Get (Line : String_Access; Loc : Match_Location) return String is
   begin
      if Loc = No_Match then
         return "";
      else
         return Line (Loc.First .. Loc.Last);
      end if;
   end Get;

   -------------
   -- Unquote --
   -------------

   function Unquote (Filename : String) return String is
      use Ada.Strings.Unbounded;

      Result   : Unbounded_String;
      In_Quote : Boolean := False;
      --  True when we just met a double quote inside a quoted filename. False
      --  otherwise.

   begin
      if Filename (Filename'First) /= '"' then
         return Filename;
      else
         --  To unquote, just copy the string removing consecutive double
         --  quotes.

         for C of Filename (Filename'First + 1 .. Filename'Last - 1) loop
            if C = '"' then
               if not In_Quote then
                  Append (Result, C);
               end if;
               In_Quote := not In_Quote;
            else
               Append (Result, C);
            end if;
         end loop;
         return To_String (Result);
      end if;
   end Unquote;

   ----------
   -- Load --
   ----------

   procedure Load
     (Repository   : in out Repository_Type;
      Library_File : GNATCOLL.VFS.Virtual_File;
      Library_Info : in out Library_Info_Type)
   is

      Cur_Source       : Source_File_Index := No_Source_File;
      Cur_Usage_Source : Source_File_Index := No_Source_File;
      Cur_Def          : Definition_Index := No_Definition;

      procedure Load_Usages (Usages : String_List);
      --  Parse usages and add them to the library info

      procedure Load_Usages (Usages : String_List) is
         Matches  : Match_Array (0 .. 4);
         Ref_Type : Character;
      begin
         for Usage of Usages loop
            Match (Use_Regexp, Usage.all, Matches);

            if Matches (0) /= No_Match then

               --  If a source file number is present, it targets this usage
               --  and the next ones: remember it.

               if Matches (1) /= No_Match then
                  Cur_Usage_Source :=
                     Valid_Source_File_Index'Value
                       (Usage (Matches (1).First .. Matches (1).Last - 1));
               end if;

               Ref_Type := Usage (Matches (3).First);

               if XRef_Allow (Ref_Type) then
                  --  Avoid xrefs that just indicate syntax constructs, not
                  --  really entity names.

                  Library_Info.Usages.Append
                    ((Sloc =>
                        (Cur_Usage_Source,
                         Positive'Value (Get (Usage, Matches (2))),
                         Positive'Value (Get (Usage, Matches (4)))),
                      Entity => Cur_Def));
               end if;
            end if;
         end loop;
      end Load_Usages;

      Content    : String_Access := Read_File (Library_File);
      Lines      : String_List_Access := Split (Content.all, ASCII.LF);
      Words      : String_List_Access := null;
      Matches    : Match_Array (0 .. 5);
   begin
      Free (Content);
      Library_Info.File_Table.Clear;
      Library_Info.Definitions.Clear;
      Library_Info.Usages.Clear;

      --  TODO??? Check that this is a valid Library File

      Parse_Lines : for Line of Lines.all loop
         case Line (Line'First) is

            --  Header lines: skip

            when 'V' | 'A' | 'M' | 'P' | 'R' | 'U' | 'W' | 'Z' => null;

            --  Dependency line: register the corresponding file in the
            --  dedicated table.

            when 'D' =>
               Match (D_Regexp, Line.all, Matches, Line'First + 2);
               if Matches (1) /= No_Match then
                  declare
                     use Source_File_Maps;
                     use Full_Filename_Maps;

                     File : constant Virtual_File :=
                        Create (+Unquote (Get (Line, Matches (1))));
                  begin
                     Library_Info.File_Table.Append
                       (Resolve_Source_File (Repository, File));
                  end;
               end if;

            --  X line: change the current source file

            when 'X' =>
               Match (X_Regexp, Line.all, Matches, Line'First + 2);
               if Matches (0) /= No_Match then
                  Cur_Source := Source_File_Index'Value
                    (Get (Line, Matches (1)));
               end if;

            --  New definition, and maybe associated usages

            when '0' .. '9' =>
               --  Skip it if there is no usage: it will be useless for us

               Match (Decl_Regexp, Line.all, Matches);
               if Matches (0) /= No_Match then
                  Words := Split
                    (Line (Matches (0).Last + 1 .. Line'Last),
                     ' ');
                  if Words'Length > 1 then
                     --  Create a new definition

                     Library_Info.Definitions.Append
                       ((Sloc =>
                          (Cur_Source,
                           Positive'Value (Get (Line, Matches (1))),
                           Positive'Value (Get (Line, Matches (3)))),
                         Name =>
                           Find
                             (Entity_Names, Get (Line, Matches (5)))));

                     --  By default, next usages are relative to the current
                     --  source.

                     Cur_Usage_Source := Cur_Source;
                     Cur_Def := Library_Info.Definitions.Last_Index;

                     Load_Usages (Words (Words'First .. Words'Last));
                  end if;
               end if;
               Free (Words);

            when '.' =>
               Words := Split (Line (Line'First + 2 .. Line'Last), ' ');
               Load_Usages (Words.all);
               Free (Words);

            when others =>
               exit Parse_Lines;
         end case;
      end loop Parse_Lines;

      Free (Lines);
   end Load;

end Library_Files;
