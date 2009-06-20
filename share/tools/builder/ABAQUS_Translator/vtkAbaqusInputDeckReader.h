/*=========================================================================

  Program:   Visualization Toolkit
  Module:    $RCSfile: vtkWindowLevelLookupTable.cxx,v $

  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
  All rights reserved.
  See Copyright.txt or http://www.kitware.com/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#ifndef __vtkAbaqusInputDeckReader_h
#define __vtkAbaqusInputDeckReader_h

#include "vtkUnstructuredGridAlgorithm.h"

/** @class vtkAbaqusInputDeckReader

  Given an Abaqus input deck produced by Patran, scan the file and produce 
  an unstructured grid dataset to represent the geometric model defined 
  therein We are designing this specifically to work with the quadratic 
  hexahedra that the engineers are using for their continuum modeling,
  supporting other cell types is not strictly required, however it has 
  been written to be intentionally agnostic of cell type, relying on the 
  ability of VTK cells to report how many nodes they should have and so on.

  Abaqus Input decks (*.inp files) produced by MSC.Patran contain code for 
  a geometric model including
  node and node set definitions and element and element set definitions.  
  This class reads from a text file only.

  keywords for node and node set definitions are:
  *node, *ngen, *nfill, *nset, *ncopy, *nmap
  keywords for element and element set definitions are:
  *element, *elgen, *elset, *elcopy

  current supported or partially supported keywords : 
    heading, element, type, elset, nset, nodes, generate

  here are the parts of an input deck I was working from when I wrote this - Budd Hirons

  @verbatim

  *HEADING
  ABAQUS job created on 11-Apr-05 at 16:08:45
  **    [ <-- a double asterisc appears to delineate sections ]
  *NODE
     1,        1874.5,       1486.32,       301.339
[ node number, x, y, z ]
     2,        1909.5,       1477.69,       376.566
     3,       1956.14,       1456.78,       442.535
     4,       1980.95,       1537.82,       424.973
    ...
[ ignored for now? ]
  *TRANSFORM, TYPE=C, NSET=CID3
       152.1,        893.,     -8392.5,     152.475,     893.134,    -8391.58
  **
[ might as well collect nodesets since it appears pretty easy...]
  *NSET, NSET=CID3
     417,     418,     419,     771,     772,     800,     801,     802,
    ...
[ element definition, first number is element number, then a list of ordered nodes
  the quantity of nodes can be determined from the TYPE value, C3 is 3D, D20 is 20 nodes
  elset lists the element set name assigned from a Patran group definition ]

  **
  *ELEMENT, TYPE=C3D20, ELSET=LAMINA_PROPS
       1,       1,       3,       7,       8,      81,      71,      74,
      83,       2,       4,       6,       5,      87,      73,      56,
      92,      45,      39,      46,      50
      ...
[  2D types  DC2D4 
  3D types  DC3D8, DC3D6, C3D20 (note the =C3D20 instead of =DC3D20, apparently a versioning difference?) ]

  **
  ** FE_Solid_1_Superior  [anything after a ** must be treated as a comment as well
  **
  *NSET, NSET=FE_SOLID_1_SUPERIOR, GENERATE
       1,       3,       1
      39,      39,       1
    ...

[  this is a facility to ubild sets without explicitly listing  every included element or node id,
  in this case : generate an element set from id 50 to id 427 inclusive, stepping 1 id at a time] 
  **
  ** FE_Solid_1_Superior
  **
  *ELSET, ELSET=FE_SOLID_1_SUPERIOR, GENERATE
      50,     427,       1
  ...

[then there are things like this which we probably don't have to worry about as well
  including section and material definitions, although Boundary may be important ]
  **
  ** Lamina_Props
  **
  *SOLID SECTION, ELSET=LAMINA_PROPS, MATERIAL=LAMINA_ISO
        1.,
  **
  ** Sclera_Props
  **
  *SOLID SECTION, ELSET=SCLERA_PROPS, MATERIAL=SCLERA_ISO
        1.,
  **
  ** Lamina_ISO
  ** Date: 11-Apr-05           Time: 16:01:56
  **
  *MATERIAL, NAME=LAMINA_ISO
  **
  *DENSITY
        1.,
  **
  *ELASTIC, TYPE=ISO
       1.E-5,        0.45
  **
  ** Sclera_ISO
  ** Date: 11-Apr-05           Time: 16:01:56
  **
  *MATERIAL, NAME=SCLERA_ISO
  **
  *DENSITY
        1.,
  **
  *ELASTIC, TYPE=ISO
     3.07E-5,        0.45
  **
  ** Equator_BCs
  **
  *BOUNDARY, OP=NEW
     417, 2,,          0.
     417, 3,,          0.
     418, 2,,          0.
  ...
  **
  ** Step  1, Default Static Step
  ** LoadCase, Default
  **
  *STEP, AMPLITUDE=RAMP, PERTURBATION
[everything between STEP and END STEPwe will ignore, since this is load definition and output requirements
  for abaqus to worry about, we are interested in teh geometry only at this point ]
  Linear Static Analysis
  **
  This load case is the default load case that always appears
  **
  *STATIC
  **
  *NSET, NSET=EQUATOR_BCS
     417,     418,     419,     771,     772,     800,     801,     802,
    1143,    1144,    1183,    1184,    1185,    1456,    1457,    1566,
    1567,    1568,    1839,    1840,    1949,    1950,    1951,    2222,
    2310,    2357,    2358,    2359,    2605,    2608,    2968,    2971,
    2973,    3067,    3069,    3098,    3099,    3100,    3708,    3753,  
  ...
[ although any node set definitions in here are probably still interesting...]
  **
  *END STEP
  @endverbatim
*/

class VTK_IO_EXPORT vtkAbaqusInputDeckReader : public vtkUnstructuredGridAlgorithm
{
public:
  static vtkAbaqusInputDeckReader* New();
  vtkTypeRevisionMacro(vtkAbaqusInputDeckReader,vtkUnstructuredGridAlgorithm);
  void PrintSelf(ostream& os, vtkIndent indent);

  // Description:
  // Specify the file name of the ABAQUS Input Deck file to read.
  vtkSetStringMacro(FileName);
  vtkGetStringMacro(FileName);

protected:
  vtkAbaqusInputDeckReader();
  ~vtkAbaqusInputDeckReader();
  int RequestInformation(vtkInformation *, vtkInformationVector **, vtkInformationVector *);
  int RequestData(vtkInformation *, vtkInformationVector **, vtkInformationVector *);

  char *FileName;

private:
  vtkAbaqusInputDeckReader(const vtkAbaqusInputDeckReader&);  // Not implemented.
  void operator=(const vtkAbaqusInputDeckReader&);  // Not implemented.
};

#endif
