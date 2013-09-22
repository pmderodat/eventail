with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;

with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

package Repositories is

   --  Eventail bundles source code for one repository. A repository is just a
   --  set of project groups, and each project group can gather multiple
   --  GPR projects.

   --  Note: gathering multiple GPR projects into a project group is not
   --  possible right now, but is expected to be implemented some day. This
   --  feature would add recursively projects from the whole project tree,
   --  stopping at projects that already belong to other project groups.

   type Repository_Type;
   type Repository_Access is access Repository_Type;
   type Project_Group_Type;
   type Project_Group_Access is access Project_Group_Type;

   package Project_Group_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Virtual_File,
      Element_Type => Project_Group_Access);
   --  Each project group is identified by its root project GPR file

   type Repository_Type is record
      Env            : Project_Environment_Access := null;
      Project_Groups : Project_Group_Maps.Map;
   end record;

   type Project_Group_Type is record
      Tree : Project_Tree;
   end record;

   package Project_File_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Virtual_File);

   procedure Load
     (Project_Roots : Project_File_Vectors.Vector;
      Repository    : out Repository_Type);
   --  Load a list of projects in a repository

   procedure Free (Repository : out Repository_Type);
   --  Free a repository and all projects it contains

end Repositories;
