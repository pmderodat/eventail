with GNATCOLL.VFS; use GNATCOLL.VFS;

with Repositories; use Repositories;

package Bundling is

   procedure Setup_Repository (Repository : in out Repository_Type);
   --  Setup repository-level bundle files: like the general index, style
   --  sheets, project directories.

   procedure Bundle_Project_Group
     (Repository    : in out Repository_Type;
      Project_Group : Project_Group_Type);
   --  Generate the part of the bundle for a project group: the project group
   --  index.

   procedure Bundle_Source_File
     (Repository    : in out Repository_Type;
      Project_Group : Project_Group_Type;
      Source_File   : Virtual_File;
      Source_Info   : Source_File_Type);
   --  Generate the part of the bundle for a single source file

   procedure Bundle (Repository : in out Repository_Type);
   --  Generate the whole bundle

end Bundling;
