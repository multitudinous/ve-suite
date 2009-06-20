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
#include "vtkAbaqusInputDeckReader.h"

#include "vtkObjectFactory.h"
#include "vtkAbaqusElementModel.h"
#include "vtkPoints.h"
#include "vtkErrorCode.h"
#include "vtkDoubleArray.h"
#include "vtkIdTypeArray.h"
#include "vtkDataArrayCollection.h"
#include "vtkPointData.h"
#include "vtkCell.h"
#include "vtkCellArray.h"
#include "vtkEmptyCell.h"
#include "vtkStdString.h"
#include "vtkInformation.h"
#include "vtkInformationVector.h"

#include <vtkstd/vector>
#include <vtkstd/map>
#include <vtkstd/utility>
#include <assert.h>
#include <vtksys/SystemTools.hxx>

char const * const comma = ",";
char const * const star = "*";
char const * const wspc = " \t\n";

//----------------------------------------------------------------------------

//forward internal declares using our vector and iterators
//  which all need to be hidden from vtk headers, esp for the wrapping

/** Base vector for caching the file in memory, doing it this way will cost us some memory for a few moments
  but allows us to not develop a state machine for parsing the file linearly */
typedef vtkstd::vector<vtkStdString> InputDeckBase;
/** Class we will instantiate to cache the file */
class InputDeck : public InputDeckBase {};

/** Base map for ordering the elements by abaqus input id.
  The key is the element ID from the input deck,
  The value is a pair, 
    the first is the array index of the element id
    the second is the cell type of the abaqus element
 */
typedef vtkstd::map<vtkIdType, vtkstd::pair<vtkIdType,vtkIdType> > ElementMapBase;
/** Class we will instantiate to order the Abaqus elements */
class AbaqusIdMap : public ElementMapBase {};

//----------------------------------------------------------------------------

class vtkAbaqusReaderHelper
{
public:
  static vtkIdType CountTokens( InputDeckBase::iterator start, InputDeckBase::iterator end );
  static vtkIdType TokenizeToArray( InputDeckBase::iterator start, InputDeckBase::iterator end, vtkDataArray *target );

  /** @brief Find the section mark that includes incString, set target iterator to that line in the InputDeck */
  static vtkIdType FindSection( InputDeckBase::iterator seekStart, InputDeckBase::iterator &secStart, InputDeckBase::iterator &secEnd, const char *secStr, InputDeck &inp );

  /** @brief Load explicit node section */
  static int LoadNodes( InputDeckBase::iterator start, InputDeckBase::iterator end, vtkIdType tokens, vtkPoints *nodes, vtkDoubleArray *scalars, double defaultScalar = 1.0 );

  /** @brief Generate a named node or element set */
  static int BuildGeneratedSet( InputDeckBase::iterator start, InputDeckBase::iterator end, vtkIdTypeArray *lst );

  /** @brief Return VTK cell type from Abaqus element code */
  static vtkIdType GetVTKCellTypeFromAbaqusType( const char *ct, vtkIdType &nodeCount );
  static vtkIdType CellNodeCountByType( int cellType );
  static vtkCell *NewVTKCellByType( int cellType );
  static vtkCell *NewVTKCellByType( int cellType, vtkIdType &nodeCount );
};

//----------------------------------------------------------------------------


/** Utility function to count tokens in a given vector of strings */
vtkIdType vtkAbaqusReaderHelper::CountTokens(InputDeckBase::iterator start, 
                                             InputDeckBase::iterator end )
{
  InputDeckBase::iterator iter;
  vtkStdString::size_type s,e,sz;
  vtkIdType comps, acc;
  comps = acc = 0;
  
  for( iter = start; iter <= end; ++iter ) 
  {
    //how many lines are there?
    acc++;

    vtkStdString &str = *iter;
    sz = str.size();
    s = e = 0;

    //how many components on this line?
    while( s < sz )
    {
      while( (s < sz) 
        && (( strchr(wspc, str[s]) != NULL ) 
          || ( strchr(comma, str[s]) != NULL )) ) 
        ++s;
      if( s == sz ) break;
      e = s;
      while( (e < sz) && (( strchr(wspc, str[e]) == NULL ) 
          && ( strchr(comma, str[e]) == NULL )) ) 
        ++e;
      comps++;  
      s = e;
    }    
  }
  return comps;
}

//----------------------------------------------------------------------------

/** Utility function to tokenize strings into the paramed array.
  This function uses InsertNextTuple, so MaxId need not be set, and in fact it will grow the array
  as necessary.  It would be smart to have it allocated to large enough so that doesn't need to happen tho.
  */
vtkIdType vtkAbaqusReaderHelper::TokenizeToArray( InputDeckBase::iterator start, InputDeckBase::iterator end, vtkDataArray *target )
{
  InputDeckBase::iterator iter;
  vtkStdString::size_type s,e, sz;
  char *stop = NULL;
  vtkIdType rval = -1;

  for( iter = start; iter <= end; ++iter )
  {
    vtkStdString &str = *iter;
    sz = str.size();
    s = e = 0;

    //extract all the numbers ignoring whitespace and commas
    while( s < sz )
    {
      while( (s < sz) && (( strchr(wspc, str[s]) != NULL ) || ( strchr(comma, str[s]) != NULL )) ) 
        ++s;
      if( s == sz ) break;
      e = s;
      while( (e < sz) && (( strchr(wspc, str[e]) == NULL ) && ( strchr(comma, str[e]) == NULL )) ) 
        ++e;

      //vtkDataArray takes a double in this method so are generalized away from real subclass type
      const double k = strtod( (str.substr(s, e-s)).c_str(), &stop );
      rval = target->InsertNextTuple( &k );
      //cout << rval << " : " << k << endl;
      s = e;
    }
  }
  return rval;
}

//----------------------------------------------------------------------------

/** LoadNodes

  given a start and an end position in the input deck, read the numbers into a vtkPoints object.
  Abaqus Input Deck node points are 1-based arrays if ids, vtkPoints is a 0 based, 
  
  So what we are doing is finding the largest id and the smallest id and allocating enough points to handle that

  Expects the start and end to point only to lines that contain node ids and coords.

  Sets a default scalar for each pointe id as 1.0...

  @return the number of points

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
  **

  @endverbatim

  */
int vtkAbaqusReaderHelper::LoadNodes( InputDeckBase::iterator start, InputDeckBase::iterator end, vtkIdType tokens, vtkPoints *nodes, vtkDoubleArray *scalars, double defaultScalar )
{
  if( !nodes )
  {
    cerr << "invalid node point set in LoadNodes" << endl;
    return 0;
  }

  //get numerical values for all the numbers in the nodes section
  vtkDoubleArray *nums = vtkDoubleArray::New();
    nums->Initialize();
    nums->SetNumberOfComponents(1);
    //estimate the required size to try to avoid 
    //  array internal allocation and memcpy when about to overrun
    nums->Allocate(tokens);

    TokenizeToArray( start+1, end, nums );

    //iterate the 0th, 4th, 8th, etc to find the smallest and largest node id
    double min, max, curr;
    max = nums->GetDataTypeMin();
    min = nums->GetDataTypeMax();
    for( int j=0; j<nums->GetMaxId(); j+=4 )
    {
      curr = nums->GetTuple1( j );
      if( curr < min ) min = curr;
      if( curr > max ) max = curr;
    }
  
    //now initialize to encompass our largest and smallest point ids
    nodes->Initialize();
    nodes->SetDataTypeToDouble();
    nodes->SetNumberOfPoints( static_cast<int>(max + 1) );

    scalars->Initialize();
    scalars->SetNumberOfValues( static_cast<int>(max + 1));

    //zero out all possible points
    for( int k=0; k<nodes->GetNumberOfPoints(); k++ )
    {
      nodes->SetPoint( k, 0.0, 0.0, 0.0 );
      scalars->SetValue( k, 0.0 );
    }

    //using pointer to data and a set method that uses no range checking, fast as we can get here
    double *val = static_cast<double*>(nums->GetVoidPointer(0));
    for( int i=0; i<nums->GetMaxId(); i+=4 )
    {
      nodes->SetPoint( static_cast<vtkIdType>(*(val+i)), *(val+i+1), *(val+i+2), *(val+i+3) );
      //cout << static_cast<vtkIdType>(*(val+i)) << " : " << *(val+i+1) << " , " << *(val+i+2) << " , " << *(val+i+3) << endl;
      scalars->SetValue( static_cast<vtkIdType>(*(val+i)), defaultScalar );
    }
  nums->Delete();

  return nodes->GetNumberOfPoints();
}

//----------------------------------------------------------------------------

/**  Given a start and an end, generate the element or node set from the input deck.

  @verbatim

  **
  ** FE_Solid_1_Superior  [anything after a ** must be treated as a comment as well
  **
  *NSET, NSET=FE_SOLID_1_SUPERIOR, GENERATE
       1,       3,       1
      39,      39,       1
    ...

[  this is a facility to build sets without explicitly listing every included element or node id,
  in this case : generate an element set from id 50 to id 427 inclusive, stepping 1 id at a time] 
  **
  ** FE_Solid_1_Superior
  **
  *ELSET, ELSET=FE_SOLID_1_SUPERIOR, GENERATE
      50,     427,       1
  ...

  @endverbatim
  */
int vtkAbaqusReaderHelper::BuildGeneratedSet( InputDeckBase::iterator start, InputDeckBase::iterator end, vtkIdTypeArray *lst )
{
  if( !lst )
  {
    cerr << "Invalid id list in BuildGeneratedElementSet" << endl;
    return 0;
  }

  vtkIdType acc, acc2, i, j;
  vtkIdType tokens = vtkAbaqusReaderHelper::CountTokens( start, end );  
  vtkIdTypeArray *rawIds = vtkIdTypeArray::New();
  rawIds->Allocate( tokens );
  vtkAbaqusReaderHelper::TokenizeToArray( start, end, rawIds );

  //first count the number of ids we will be storing
  acc = 0;
  for( i=0; i<rawIds->GetSize(); i+=3 )
    {
    //cout << "Line : " << (i/3) << endl;
    //cout << "  From " << rawIds->GetValue(i) << " to " << rawIds->GetValue(i+1) << " in steps of " << rawIds->GetValue(i+2) << endl;

    acc2 = 0;
    for( j=rawIds->GetValue(i); j<=rawIds->GetValue(i+1);  j+=rawIds->GetValue(i+2) )
      {
      acc++;
      acc2++;
      }

    //cout << "  Is " << acc2 << " ids, and " << acc << " total so far" << endl;
    }

  //allocate
  lst->Initialize();
  lst->SetNumberOfValues( acc );
  acc = 0;

  //now generate
  for( i=0; i<rawIds->GetMaxId(); i+=3 )
    {
    for( j=rawIds->GetValue(i); j<=rawIds->GetValue(i+1);  j+=rawIds->GetValue(i+2) )
      {
      lst->SetValue( acc++, j );
      }
    }
  rawIds->Delete();
  return lst->GetNumberOfTuples();
}

//----------------------------------------------------------------------------

/** Converts string symbol for Abaqus Element types to a VTK constant as listed in vtkCellType.h.

  I generally have only written translations for an assortment of shell and solid geometric types,
  things like Heat Transfer types are irrelevant for my purposes, Though I have included all the types
  I could round up for completeness sake - Budd.

  @verbatim

  Beam types B21 B21H B22 B22H B23 B23H B31 B31H B32 B32H B33 B33H B34 
  
  Shell types Triangle linear STRI3
        Triangle linear STRI35
        Triangle linear S3RF 
        Quadrangle linear S4R
        Quadrangle linear S4R5
        Quadrangle linear S4RF

        Triangular quadratic STRI65
        Quadrangle quadratic S8R
        Quadrangle quadratic S8R5
        Quadrangle quadratic S8RT
        Quadrangle quadratic S9RS
  Solid Types
        Tetra linear C3D4
        Tetra linear C3D4H
        Wedge linear C3D6H
        Wedge linear C3D6
        Hexa linear C3D8
        Hexa linear C3D8R
        Hexa linear C3D8H
        Hexa linear C3D8HT
        Hexa linear C3D8RH

        Tetra quadratic C3D10
        Tetra quadratic C3D10H
        Wedge quadratic C3D15H
        Wedge quadratic C3D15
        Wedge quadratic C3D15V
        Wedge quadratic C3D15VH
        Hexa quadratic C3D20
        Hexa quadratic C3D20R
        Hexa quadratic C3D20T
        Hexa quadratic C3D20RT
        Hexa quadratic C3D27
        Hexa quadratic C3D27H
        Hexa quadratic C3D27RH

   Heat Transfer Types
        DS4, DS8,DC3D4, DC3D6, DC3D8, DC3D10, DC3D15, DC3D20, 
        DCAX3, DCAX4, DCAX6, DCAX8, DC1D2, DC1D3, DC2D3, 
        DC2D4, DC2D6, DC2D8, DSAX1, DSAX2

   Axisymmetric Types
        CAX3, CAX3H, CAX4, CAX4H, CAX4R, CAX4RH, CAX6, 
        CAX6H, CAX8, CAX8H, CAX8R, CAX8RT, CAX8T

   Truss Types
        C1D2, C1D2H, C1D2T, C1D3, C1D3H, C1D3T

   Miscellaneous Types
        DASHPOTA, SPRINGA

  @endverbatim

  The first cell we want to support is quad hexes.

*/
vtkIdType vtkAbaqusReaderHelper::GetVTKCellTypeFromAbaqusType( const char *ct, vtkIdType &nodeCount )
{
  if( strncmp( ct, "C3D20", 5 ) == 0 ||
    strncmp( ct, "C3D20R", 6 ) == 0 ||
    strncmp( ct, "C3D20T", 6 ) == 0 ||
    strncmp( ct, "C3D20RT", 7 ) == 0)
  {
    nodeCount = 20;
    return VTK_QUADRATIC_HEXAHEDRON;
  }

  cerr << " Cell Type " << ct << " not yet supported." << endl;
  return -1;
}

//----------------------------------------------------------------------------

vtkIdType vtkAbaqusReaderHelper::CellNodeCountByType( int cellType )
{
  switch( cellType )
  {
  case VTK_EMPTY_CELL:
    {
      return 0;
    }
  case VTK_QUADRATIC_HEXAHEDRON:
    {
      return 20;
    }
    break;
  default:
    {
      cerr << " Cell Type " << cellType << " no yet supported." << endl;
      return 0;
    }
  }
}



//----------------------------------------------------------------------------

/** FindSection
  
  Find the section market that includes incString 
  
  Sets the secStart iterator to point to the line where the secStr was found and secEnd to the end
  
  returns number of tokens in the section
  */
vtkIdType vtkAbaqusReaderHelper::FindSection( InputDeckBase::iterator seekStart, InputDeckBase::iterator &secStart, InputDeckBase::iterator &secEnd, const char *secStr, InputDeck &inp )
{
  InputDeckBase::iterator iter;
  vtkStdString::size_type in;
  int bInSection = 0;

  for( iter = seekStart; iter != inp.end(); ++iter )
  {
    //we have a section marker
    //check to see if it is a set of some kind
    if( iter->c_str()[0] == *(star) )
    {
      //cerr << "FindSection found : " << *iter << endl;

      if( bInSection )
      {
        //set end to the end of this element block
        secEnd = iter-1;

        //count the tokens, 
        return vtkAbaqusReaderHelper::CountTokens( secStart+1, secEnd );
      }
      else
      {
        vtkStdString &hdr = *iter;
        in = (hdr.find( secStr, 0 ));
        if( in < hdr.length() )
        {
          //we have found the start of the section
          if( bInSection )
          {
            cerr << "FindSection : Found start of section block while still in explicit node set block" << endl;
          }

          secStart = iter;
          bInSection = 1;
        }
      }
    }
  }

  secEnd = inp.end();
  return 0;
}

//----------------------------------------------------------------------------

vtkCxxRevisionMacro(vtkAbaqusInputDeckReader, "$Revision$");
vtkStandardNewMacro(vtkAbaqusInputDeckReader);

//----------------------------------------------------------------------------
vtkAbaqusInputDeckReader::vtkAbaqusInputDeckReader()
{
  this->FileName = NULL;
  this->SetNumberOfInputPorts(0);
}

//----------------------------------------------------------------------------
vtkAbaqusInputDeckReader::~vtkAbaqusInputDeckReader()
{
  this->SetFileName(0);
}

//----------------------------------------------------------------------------
void vtkAbaqusInputDeckReader::PrintSelf(ostream& os, vtkIndent indent)
{
  if( this->FileName )
    os << indent << "File Name : " << this->FileName << endl;

  this->Superclass::PrintSelf(os,indent);
}

//----------------------------------------------------------------------------

//vtkAbaqusElementModel *vtkAbaqusInputDeckReader::GetOutput()
//{
//  return( vtkAbaqusElementModel::SafeDownCast(vtkUnstructuredGridSource::GetOutput()) );
//}

//----------------------------------------------------------------------------

/** Load the grid data from the input deck at FileName.

  The entire input deck is cached into a vector and parsed in memory.  This is best since the line
  length varies across the input deck, meaning no regular pointer increments for lines.

  In order to handle the fact that abaqus element ids may or may not be contiguous, there may be 
  empty cells inserted into the final dataset.  Also, we have to handle potential variance of 
  cell types, perhaps even adjacent to each other.

  The solution at the moment is to find the largest id in the input, and allocate 
  enough space to encompass that.  Then we we actually set the cells into the output, I 
  stuff a VTK_EMPTY_CELL into the slots that have no real cell in the input deck.
  
  */
int vtkAbaqusInputDeckReader::RequestData(
  vtkInformation *vtkNotUsed(request),
  vtkInformationVector **vtkNotUsed(inputVector),
  vtkInformationVector *outputVector)
{
//  if( this->NumberOfOutputs < 1 )
//    {
//    this->SetOutput( vtkAbaqusElementModel::New() );
//    }
//  vtkAbaqusElementModel *out = 
//    vtkAbaqusElementModel::SafeDownCast(this->GetOutput());

  // get the info object
  vtkInformation *outInfo = outputVector->GetInformationObject(0);

  // get the ouptut
  vtkAbaqusElementModel *out = vtkAbaqusElementModel::SafeDownCast(
    outInfo->Get(vtkDataObject::DATA_OBJECT()));
  if( !out )
    {
    vtkErrorMacro("Ack!! no output!!");
    }
  
  vtkDebugMacro( << "Reading AbaqusInputDeck file");
  
  //read and cache the file so we can jump around by line index
  InputDeck inp;

#ifdef WIN32
  ifstream ifile( this->FileName, ios::in | ios::nocreate);
#else
  ifstream ifile( this->FileName, ios::in|ios::binary);
#endif
  if( !ifile )
    {
    this->SetErrorCode(vtkErrorCode::FileNotFoundError);
    vtkErrorMacro( << "Specified filename not found : " << this->FileName );
    return 0;
    }

  inp.clear();
  vtkStdString line;
  while( vtksys::SystemTools::GetLineFromStream(ifile, line) )
    {
    if( line[line.size()-1] == '\r' )
      {
      line.erase( line.size()-1 );
      }
    inp.push_back( line.c_str() );
    }

  //cleanup and done
  ifile.clear();
  ifile.close();

  InputDeckBase::iterator secStart;
  InputDeckBase::iterator secEnd;
  InputDeckBase::iterator iter;
  
  AbaqusIdMap AbqMap;
  ElementMapBase::iterator idIter;
  
  vtkStdString::size_type s,e,v;

  vtkPoints *nodes = NULL;
  vtkDoubleArray *nodeScalars = NULL;
  vtkIdTypeArray *rawElements = NULL;
  vtkIdTypeArray *idSet = NULL;
  vtkIdType tokens, cellType, nodeCount, sIdx, eIdx, idx;
  vtkIdType min, max, curr;

  //get the file header
  tokens = vtkAbaqusReaderHelper::FindSection( inp.begin(), secStart, secEnd, "*HEADING", inp );
  iter = secStart+1;
  out->SetHeading( (iter)->c_str() );

  //load the nodes to a vtkPoints object
  tokens = vtkAbaqusReaderHelper::FindSection( iter, secStart, secEnd, "*NODE", inp );  
  nodes = vtkPoints::New();
    nodeScalars = vtkDoubleArray::New();
      idx = vtkAbaqusReaderHelper::LoadNodes( secStart, secEnd, tokens, nodes, nodeScalars );
      vtkDebugMacro( << nodes->GetNumberOfPoints() << " node points loaded from Input Deck." );
      out->SetPoints( nodes );
      out->GetPointData()->SetScalars( nodeScalars );
    nodeScalars->Delete();
  nodes->Delete();
  

  //
  //Get all the explicit element sections tokenized so we can build cells in teh out dataset
  //
  //we also need to prepare to handle the fact that each section, while being composed of
  //  a single cell type, may not have contiguous ids
  //  also we should be pepared to handle a different cell type for each section
  //  
  //create a map that is keyed on element id, and the value will be the array index of that same element id
  //create a single-component array that lists the element id and its subsequent ordered point ids
  //
  //rawElements will accumulate everything in all of the explicit element sets that we find in the input deck
  rawElements = vtkIdTypeArray::New();
    rawElements->Initialize();
    rawElements->SetNumberOfComponents(1);
    rawElements->Allocate(1);

    tokens = cellType = nodeCount = sIdx = eIdx = 0;
    
    while( secEnd != inp.end() )
    {
      tokens = cellType = nodeCount = 0;
      iter = secEnd;
      tokens = vtkAbaqusReaderHelper::FindSection( iter, secStart, secEnd, "*ELEMENT, TYPE=", inp );
      if( secEnd >= inp.end() )
        break;

      //find and the node count for this block
      vtkStdString &hdr = *secStart;
      s = hdr.find( "TYPE=", 0 );
      e = hdr.length();
      if( s+5 < e )
        cellType = vtkAbaqusReaderHelper::GetVTKCellTypeFromAbaqusType( (hdr.substr( s+5, e ) ).c_str(), nodeCount );
      else
      {
        //fatal error?
        vtkErrorMacro( << "Could not find element type!" );
      }
      
      idSet = vtkIdTypeArray::New();
        //get the set name if there is one, should be last in line
        v = (hdr.find( "ELSET=", 0 ));
        if( v < (e-6) )
        {
          idSet->SetName( (hdr.substr( v+6, e ) ).c_str() );
        }
        else
        {
          //give it a generic name
          vtkStdString tmpName = "ELSET_";
          char *buff = new char[32];
            sprintf( buff, "%06ld", static_cast<long>(eIdx) );
            tmpName += buff;
          delete [] buff;
          vtkWarningMacro( << "Could not find element set name, using a generic name : " << tmpName.c_str() );
          idSet->SetName( tmpName.c_str() );
        }
        
        //extend or allocate the array
        sIdx = rawElements->GetMaxId() + 1;
        if( sIdx == 0 )
        {
          rawElements->Initialize();
          rawElements->Allocate( tokens );
        }
        else
        {
          //as long as we are resizing to a larger array, 
          //  MaxId will remain the same
          rawElements->Resize( tokens + sIdx );
        }

        //now add to the array, TokenizeToArray uses InsertNextTuple which starts at MaxId + 1
        tokens = vtkAbaqusReaderHelper::TokenizeToArray( secStart + 1, secEnd, rawElements );
        //now get then last indices now that rawElements is allocated or resized
        //  and allocate the id array to collect the element ids
        eIdx = rawElements->GetMaxId();
        //the number of values for the cell id array that we will set into the output
        //  is the number of raw tokens in this section divided 
        //  by the section's celltype nodecount + 1 for the node id
        long idCount = ((eIdx - sIdx + 1) / (nodeCount+1));
        idSet->SetNumberOfValues( idCount );
        std::cerr << "Number of ID values : " << idCount << std::endl;

        idx = 0;
        for( int i=sIdx; i<=eIdx; i+=(nodeCount+1) )
        {
          assert( idx < idSet->GetNumberOfTuples());
          assert( i < rawElements->GetMaxId());
          vtkDebugMacro( << " Adding element " << rawElements->GetValue(i)
            << " as index " << idx << " in the new cell id array." );
          AbqMap.insert( vtkstd::make_pair( rawElements->GetValue(i), 
              vtkstd::make_pair(i, cellType) ) );
          idSet->SetValue( idx++, rawElements->GetValue(i) );
        }
        out->AddElementSet( idSet );

      idSet->Delete();
      idSet = NULL;
    }

    //now iterate the map to get the largest element id,
    min = 100000;
    max = -1;
    for( idIter = AbqMap.begin(); idIter != AbqMap.end(); ++idIter )
    {
      curr = idIter->first;
      if( curr < min ) min = curr;
      if( curr > max ) max = curr;
    }
    
    //max number of cells...
    out->Allocate(max);
    //
    // then loop from 0 to the largest id
    //  at each loop, try to get the element id,
    //    if it exists in the map, 
    //      create it based on its type and 
    //      add it to the outdata
    //    if it does not exist in the map, 
    //      add an empty cell
    //
    vtkIdType empty;
    empty = 0;

    for( vtkIdType eid = 0; eid <= max; eid++ )
    {
      nodeCount = 0;
      idx = -1;
      vtkIdType *rp = NULL;

      idIter = AbqMap.find(eid);
      if( idIter == AbqMap.end() )
      {
        //insert an empty cell 
        cellType = VTK_EMPTY_CELL;
        nodeCount = 1;
        rp = &empty;
        vtkDebugMacro( << eid << " : <empty>");
      }
      else
      {
        //create a new cell to spec and add it
        cellType = (idIter->second).second;
        nodeCount = vtkAbaqusReaderHelper::CellNodeCountByType( cellType );
        
        idx = (idIter->second).first;
          
        //get the cell point ids into the list
        rp = rawElements->GetPointer( idx+1 );
      }

      out->InsertNextCell( cellType, nodeCount, rp );
      vtkDebugMacro( << eid << " of type " << cellType << "inserted");
    }

  rawElements->Delete();

  //add the points and build the links
  out->BuildLinks();

  //now is the time to get all the generated elset and nset groups in the file
  secStart = secEnd = inp.begin();
  while( secEnd != inp.end() )
  {
    iter = secEnd;
    tokens = vtkAbaqusReaderHelper::FindSection( iter, secStart, secEnd, "*NSET,", inp ); 
    if( secEnd == inp.end() )
      break;

    vtkStdString &hdr = *secStart;

    idSet = vtkIdTypeArray::New();
      //find the set name
      v = hdr.find( "NSET=", 0 );
      if( v < hdr.length() )
      {
        vtkStdString tmp = hdr.substr( v+5, hdr.length() );
        e = tmp.find( ", GENERATE", 0 );
        if( e < tmp.length() )
          tmp.erase( e );
        idSet->SetName( tmp.c_str() );
        vtkDebugMacro( << "Node Set Encountered : " << idSet->GetName() );
      }

      //determine which kind of set it is
      v = hdr.find( "GENERATE", 0 );
      if( v < hdr.length() )
      {
        //we have a generated set to create
        idx = vtkAbaqusReaderHelper::BuildGeneratedSet( secStart+1, secEnd, idSet );
      }
      else
      {
        //we have an explicit set, just tokenize it
        idSet->Allocate( tokens );
        idx = vtkAbaqusReaderHelper::TokenizeToArray( secStart+1, secEnd, idSet );      
      }

      vtkDebugMacro( << "  Adding Node Set : " << idSet->GetName()  );

      out->AddNodeSet( idSet );
    idSet->Delete();
    idSet = NULL;
  }

  secStart = secEnd = inp.begin();
  while( secEnd != inp.end() )
  {
    iter = secEnd;
    tokens = vtkAbaqusReaderHelper::FindSection( iter, secStart, secEnd, "*ELSET,", inp ); 
    if( secEnd == inp.end() )
      break;

    vtkStdString &hdr = *secStart;

    idSet = vtkIdTypeArray::New();
      //find the set name
      v = hdr.find( "ELSET=", 0 );
      if( v < hdr.length() )
      {
        vtkStdString tmp = hdr.substr( v+6, hdr.length() );
        e = tmp.find( ", GENERATE", 0 );
        if( e < tmp.length() )
          tmp.erase( e );
        idSet->SetName( tmp.c_str() );
        vtkDebugMacro( << "Element Set Encountered : " << idSet->GetName() );
      }

      //determine which kind of set it is
      v = hdr.find( "GENERATE", 0 );
      if( v < hdr.length() )
      {
        //we have a generated set to create
        idx = vtkAbaqusReaderHelper::BuildGeneratedSet( secStart+1, secEnd, idSet );
      }
      else
      {
        //we have an explicit set, just tokenize it
        idSet->Allocate( tokens );
        idx = vtkAbaqusReaderHelper::TokenizeToArray( secStart+1, secEnd, idSet );      
      }

      vtkDebugMacro( << "  Adding Element Set : " << idSet->GetName() );

      out->AddElementSet( idSet );
    idSet->Delete();
    idSet = NULL;
  }

  return 1;
}
  
//----------------------------------------------------------------------------

int vtkAbaqusInputDeckReader::RequestInformation(
  vtkInformation *vtkNotUsed(request),
  vtkInformationVector **vtkNotUsed(inputVector),
  vtkInformationVector *vtkNotUsed(outputVector))
{
  if ( !this->FileName )
  {
    vtkErrorMacro( << "No filename specified");
    return 0;
  }
  return 1;
}
