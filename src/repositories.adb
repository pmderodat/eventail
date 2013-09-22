with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with GNAT.Strings;

package body Repositories is

   procedure Report_Error (Msg : String);

   ------------------
   -- Report_Error --
   ------------------

   procedure Report_Error (Msg : String) is
   begin
      --  TODO??? generate appropriate warning/error

      Put_Line (Standard_Error, "Error while loading projects: " & Msg);
   end Report_Error;

   ----------
   -- Load --
   ----------

   procedure Load
     (Project_Roots : Project_File_Vectors.Vector;
      Repository    : out Repository_Type)
   is

      GNAT_Version  : GNAT.Strings.String_Access;
      Project_Group : Project_Group_Access;
      Src_Files     : File_Array_Access;
      Lib_Files     : Library_Info_Lists.List;

   begin
      --  Initialize the project environment if needed

      if Repository.Env = null then
         Initialize (Repository.Env);
         Repository.Env.Set_Path_From_Gnatls ("gnatls", GNAT_Version);
         GNAT.Strings.Free (GNAT_Version);
      end if;

      --  Then load all project files and add them to the repository

      for Prj_File of Project_Roots loop
         Project_Group := new Project_Group_Type;
         Repository.Project_Groups.Insert (Prj_File, Project_Group);

         --  TODO??? handle GNATCOLL.Projects.Invalid_Project errors.

         Project_Group.Tree.Load
           (Root_Project_Path => Prj_File,
            Env               => Repository.Env,
            Errors            => Report_Error'Access);

         --  Also load source files that are part of these projects.  Note that
         --  one single source file may belong to more than one project group.
         --  However, each source file cannot be displayed in more than one
         --  project group.

         --  Display only sources that are part of the root project

         for Recursive in False .. True loop
            Src_Files := Source_Files
              (Project   => Root_Project (Project_Group.Tree),
               Recursive => Recursive);

            for Src of Src_Files.all loop
               declare
                  Inserted : Boolean;
                  Position : Source_File_Maps.Cursor;
               begin
                  --  Actually perform allocation only if insertion is
                  --  successful.

                  Project_Group.Source_Files.Insert
                    (Src, null, Position, Inserted);
                  if Inserted then
                     Project_Group.Source_Files.Replace_Element
                       (Position,
                        new Source_File_Type'
                          (Language      =>
                              GNATCOLL.Symbols.Find
                                (Languages,
                                 Language (Info (Project_Group.Tree, Src))),
                           Project_Group => Project_Group,
                           XRef_File     => No_File,
                           Displayed     => not Recursive));

                     --  Update the full filenames index

                     Project_Group.Full_Filenames.Insert
                        (Create (Base_Name (Src)), Src);
                  end if;
               end;
            end loop;

            Unchecked_Free (Src_Files);
         end loop;

         --  Get xref files for all displayed sources

         Library_Files
           (Self                => Root_Project (Project_Group.Tree),
            Including_Libraries => False,
            List                => Lib_Files);
         for Lib_Info of Lib_Files loop
            declare
               use Source_File_Maps;

               Position : Cursor :=
                  Project_Group.Source_Files.Find (Lib_Info.Source_File);
            begin
               if Position /= No_Element then
                  Element (Position).XRef_File := Lib_Info.Library_File;

                  --  This Library File also belongs to the "other file"

                  Position := Project_Group.Source_Files.Find
                    (Project_Group.Tree.Other_File (Lib_Info.Source_File));
                  if Position /= No_Element then
                     Element (Position).XRef_File := Lib_Info.Library_File;
                  end if;
               end if;
            end;
         end loop;
         Lib_Files.Clear;

      end loop;
   end Load;

   procedure Free (Repository : out Repository_Type) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Project_Group_Type, Project_Group_Access);

   begin
      --  First, free all project groups
      for Prj of Repository.Project_Groups loop
         Free (Prj);
      end loop;
      Repository.Project_Groups.Clear;

      --  Finally, free the project environment
      Free (Repository.Env);
   end Free;

   -------------------------
   -- Resolve_Source_File --
   -------------------------

   function Resolve_Source_File
     (Repository : in out Repository_Type;
      File       : Virtual_File) return Source_File_Maps.Cursor
   is
      use Source_File_Maps;
      use Full_Filename_Maps;

      Result      : Source_File_Maps.Cursor := Source_File_Maps.No_Element;
      Cur         : Source_File_Maps.Cursor;
      Full        : Full_Filename_Maps.Cursor;
      Source_File : Virtual_File;
   begin
      for Prj_Grp of Repository.Project_Groups loop
         --  If needed, try to find the corresponding full filename

         if not Is_Absolute_Path (File) then
            Full := Prj_Grp.Full_Filenames.Find (Create (Base_Name (File)));
            if Full = Full_Filename_Maps.No_Element then
               Source_File := No_File;
            else
               Source_File := Element (Full);
            end if;
         else
            Source_File := File;
         end if;

         --  And then look for this source file.

         if Source_File /= No_File then
            Cur := Prj_Grp.Source_Files.Find (Source_File);
            if Cur /= Source_File_Maps.No_Element then
               Result := Cur;

               --  If the source file we found is displayed: this is *the* one
               --  we were looking for. Return it!

               if Element (Cur).Displayed then
                  return Cur;
               end if;
            end if;
         end if;
      end loop;

      --  If this source file is completely out of the scope of the repository,
      --  keep track of it anyway.

      if Result = Source_File_Maps.No_Element then
         Result := Repository.Out_Of_Scope_Sources.Find (File);
      end if;
      if Result = Source_File_Maps.No_Element then
         declare
            Inserted : Boolean;
         begin
            Repository.Out_Of_Scope_Sources.Insert
              (File,
               new Source_File_Type'(Unknown_Language, null, No_File, False),
               Result,
               Inserted);
            pragma Unreferenced (Inserted);
         end;
      end if;

      --  If we found no displayed source, return the last one we found

      return Result;
   end Resolve_Source_File;

   ----------
   -- Name --
   ----------

   function Name (Project_Group : Project_Group_Type) return String is
   begin
      return Name (Root_Project (Project_Group.Tree));
   end Name;

end Repositories;
