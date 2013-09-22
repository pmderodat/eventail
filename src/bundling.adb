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
      pragma Unreferenced (Repository);
      use Source_File_Maps;

      Prj_Name : constant String := Project_Group_Name (Project_Group);
      Index : HTML.Handle :=
         HTML.Start
           (Create_From_Base (+"index.html", +Prj_Name),
            Get_Current_Dir,
            Prj_Name);
   begin
      HTML.Add_Backlink (Index, "Repository index", "../index.html");
      HTML.Start_List (Index);

      for Cur in Project_Group.Source_Files.Iterate loop
         if Element (Cur).Displayed then
            declare
               Src_Name : constant String := +Base_Name (Key (Cur));
            begin
               HTML.Add_Item (Index, Src_Name, Src_Name & ".html");
            end;
         end if;
      end loop;

      HTML.Stop_List (Index);
      HTML.Add_Backlink (Index, "Repository index", "../index.html");
      HTML.Stop (Index);
   end Bundle_Project_Group;

   ------------------------
   -- Bundle_Source_File --
   ------------------------

   procedure Bundle_Source_File
     (Repository    : Repository_Type;
      Project_Group : Project_Group_Type;
      Source_File   : Virtual_File;
      Source_Info   : Source_File_Type)
   is
      pragma Unreferenced (Repository);
      pragma Unreferenced (Source_Info);

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
      use Source_File_Maps;
   begin
      Setup_Repository (Repository);

      for Prj_Grp of Repository.Project_Groups loop
         Bundle_Project_Group (Repository, Prj_Grp.all);

         for Cur in Prj_Grp.Source_Files.Iterate loop
            declare
               Src_File : constant Source_File_Access := Element (Cur);
            begin
               if Src_File.Displayed then
                  Bundle_Source_File
                    (Repository, Prj_Grp.all, Key (Cur), Src_File.all);
               end if;
            end;
         end loop;
      end loop;
   end Bundle;

end Bundling;
