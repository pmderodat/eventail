with GNATCOLL.VFS; use GNATCOLL.VFS;

with Library_Files;
with Repositories;

package HTML is

   type Handle is private;

   function Start
     (File  : Virtual_File;
      Root  : Virtual_File;
      Title : String) return Handle;
   procedure Stop (H : in out Handle);

   procedure Add_Backlink (H : in out Handle; Name, HRef : String);

   procedure Start_List (H : in out Handle);
   procedure Stop_List (H : in out Handle);
   procedure Add_Item (H : in out Handle; Name, HRef : String);

   procedure Add_Code
     (H            : in out Handle;
      Source_File  : Virtual_File;
      Source_Info  : Repositories.Source_File_Type;
      Library_Info : Library_Files.Library_Info_Type);

   procedure Setup_Media;

private

   type Handle is record
      File            : Writable_File;
      File_Path, Root : Virtual_File;
   end record;

end HTML;
