with Ada.Containers.Vectors;

with GNATCOLL.Symbols;
with GNATCOLL.VFS;

with Repositories; use Repositories;

package Library_Files is

   type Source_File_Index is new Natural;
   subtype Valid_Source_File_Index is
      Source_File_Index range 1 ..  Source_File_Index'Last;
   No_Source_File : Source_File_Index := 0;

   type Definition_Index is new Natural;
   subtype Valid_Definition_Index is
      Definition_Index range 1 ..  Definition_Index'Last;
   No_Definition : Definition_Index := 0;

   type Usage_Index is new Natural;
   subtype Valid_Usage_Index is
      Usage_Index range 1 ..  Usage_Index'Last;
   No_Usage : Usage_Index := 0;

   type Source_Location is record
      File   : Valid_Source_File_Index;
      Line   : Positive;
      Column : Positive;
   end record;

   type Definition_Type is record
      Sloc : Source_Location;
      Name : GNATCOLL.Symbols.Symbol;

      --  Not used here right now, thus not stored: type, level, renameref,
      --  instref, typeref, overref
   end record;

   type Usage_Type is record
      Sloc   : Source_Location;
      Entity : Valid_Definition_Index;
   end record;

   package File_Table_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Valid_Source_File_Index,
      Element_Type => Source_File_Maps.Cursor,
      "="          => Source_File_Maps."=");

   package Definition_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Valid_Definition_Index,
      Element_Type => Definition_Type);

   package Usage_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Valid_Usage_Index,
      Element_Type => Usage_Type);

   type Library_Info_Type is record
      File_Table  : File_Table_Vectors.Vector;
      Definitions : Definition_Vectors.Vector;
      Usages      : Usage_Vectors.Vector;
   end record;

   procedure Load
     (Repository   : in out Repository_Type;
      Library_File : GNATCOLL.VFS.Virtual_File;
      Library_Info : in out Library_Info_Type);
   --  Parse a Library File and return its xref information.

end Library_Files;
