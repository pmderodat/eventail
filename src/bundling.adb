with GNATCOLL.Projects; use GNATCOLL.Projects;

with HTML;

package body Bundling is

   function Project_Group_Name
      (Project_Group : Project_Group_Type) return String;
   --  Return the name of the root project of a project group

   ------------------------
   -- Project_Group_Name --
   ------------------------

   function Project_Group_Name
      (Project_Group : Project_Group_Type) return String
   is
   begin
      return Name (Root_Project (Project_Group.Tree));
   end Project_Group_Name;

   ----------------------
   -- Setup_Repository --
   ----------------------

   procedure Setup_Repository (Repository : Repository_Type) is
      Index : HTML.Handle :=
         HTML.Start
           (Create (+"index.html"),
            Get_Current_Dir,
            "Eventail repository");
   begin
      --  Generate the repository index
      HTML.Start_List (Index);

      for Prj_Grp of Repository.Project_Groups loop
         declare
            Prj_Name : constant String := Project_Group_Name (Prj_Grp.all);
            Dir_Name : constant Virtual_File := Create (+Prj_Name);
         begin
            HTML.Add_Item (Index, Prj_Name, Prj_Name & "/index.html");

            --  Create the project group directory, if needed

            if not Is_Directory (Dir_Name) then
               Make_Dir (Dir_Name);
            end if;
         end;
      end loop;

      HTML.Stop_List (Index);
      HTML.Stop (Index);

      --  Generate stylesheet files
      HTML.Setup_Media;
   end Setup_Repository;

   --------------------------
   -- Bundle_Project_Group --
   --------------------------

   procedure Bundle_Project_Group
     (Repository    : Repository_Type;
      Project_Group : Project_Group_Type)
   is
      Prj_Name : constant String := Project_Group_Name (Project_Group);

      Src_Files : File_Array_Access :=
         Source_Files (Root_Project (Project_Group.Tree));

      Index : HTML.Handle :=
         HTML.Start
           (Create_From_Base (+"index.html", +Prj_Name),
            Get_Current_Dir,
            Prj_Name);
   begin
      HTML.Add_Backlink (Index, "Repository index", "../index.html");
      HTML.Start_List (Index);

      for Src_File of Src_Files.all loop
         declare
            Src_Name : constant String := +Base_Name (Src_File);
         begin
            HTML.Add_Item (Index, Src_Name, Src_Name & ".html");
         end;
         Bundle_Source_File (Repository, Project_Group, Src_File);
      end loop;

      HTML.Stop_List (Index);
      HTML.Add_Backlink (Index, "Repository index", "../index.html");
      HTML.Stop (Index);
      Unchecked_Free (Src_Files);
   end Bundle_Project_Group;

   ------------------------
   -- Bundle_Source_File --
   ------------------------

   procedure Bundle_Source_File
     (Repository    : Repository_Type;
      Project_Group : Project_Group_Type;
      Source_File   : Virtual_File)
   is
      pragma Unreferenced (Repository);

      Prj_Name : constant String := Project_Group_Name (Project_Group);
      Src_Name : constant String := +Base_Name (Source_File);

      Src_Info : constant File_Info := Info (Project_Group.Tree, Source_File);

      H : HTML.Handle :=
         HTML.Start
           (Create_From_Base (+(Src_Name & ".html"), +Prj_Name),
            Get_Current_Dir,
            Prj_Name & " - " & Src_Name & " (" & Language (Src_Info) & ")");
   begin
      HTML.Add_Backlink (H, Prj_Name & " index", "index.html");
      HTML.Add_Code (H, Source_File, Language (Src_Info));
      HTML.Add_Backlink (H, Prj_Name & " index", "index.html");
      HTML.Stop (H);
   end Bundle_Source_File;

   ------------
   -- Bundle --
   ------------

   procedure Bundle (Repository : Repository_Type) is
   begin
      Setup_Repository (Repository);
      for Prj_Grp of Repository.Project_Groups loop
         Bundle_Project_Group (Repository, Prj_Grp.all);
      end loop;
   end Bundle;

end Bundling;
