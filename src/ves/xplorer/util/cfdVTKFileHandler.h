/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_VTK_FILE_HANDLER_H
#define CFD_VTK_FILE_HANDLER_H
/*!\file cfdVTKFileHandler.h
cfdVTKFileHandler API
*/

/*!\class VE_Util::cfdVTKFileHandler
*
*/
class vtkXMLFileReadTester;
class vtkDataSet;
class vtkDataObject;

#include "VE_Installer/include/VEConfig.h"
#include <string>

namespace VE_Util
{
class VE_UTIL_EXPORTS cfdVTKFileHandler
{
public:
	///Constructor
   cfdVTKFileHandler();
   ///Copy Constructor
   ///\param fh Right hand side
   cfdVTKFileHandler(const cfdVTKFileHandler& fh);
   ///Destructor
   virtual ~cfdVTKFileHandler();

   enum OutFileType{CFD_XML,VTK_CLASSIC};
   enum OutFileMode{CFD_ASCII=0,CFD_BINARY};

   ///Set the input filename
   ///\param inFile The input filename
   void SetInputFileName(std::string inFile);
   ///Set the output filename
   ///\param oFile The output filename
   void SetOutputFileName(std::string oFile);
   ///Set the output file type. Default is CFD_XML
   void SetVTKOutFileType(OutFileType type);
   ///Set the output file mode. Default is CFD_BINARY
   void SetOutFileWriteMode(OutFileMode mode);

   ///\param vtkFileName The fileName of the vtkDataSet to read in.
   ///vtkDataSet* GetDataSetFromFile(std::string vtkFileName);

   ///Get the dataobject from the file
   ///\param vtkFileName The name of the file to read in.
   vtkDataObject* GetDataSetFromFile(std::string vtkFileName);
   
   ///Write the DataObject to file
   ///\param dataObject The vtkDataObject to write
   ///\param outFileName The output filename.
   bool WriteDataSet(vtkDataObject* dataObject,std::string outFileName);

   ///Equal operator
   ///\param fh The right hand side
   cfdVTKFileHandler& operator=(const cfdVTKFileHandler& fh);
protected:
	///Read XML UnstructredGrid data
   void _getXMLUGrid();
   ///Read XML StructuredGrid data
   void _getXMLSGrid();
   ///Read xML RectilinearGrid data
   void _getXMLRGrid();
   ///Read XML Polydata
   void  _getXMLPolyData();
   ///Read MultiGroup data
   ///\param isMultiBlock Determines if the data is MultiBlock or Hierachical
   void _getXMLMultiGroupDataSet(bool isMultiBlock=true);
   ///Reader function to open an vtkImageData file
   void GetXMLImageData( void );
   ///Read old style(non-XML) vtk file
   void _readClassicVTKFile();
   ///Write old style(non-XML) vtk file
   void _writeClassicVTKFile( vtkDataObject * vtkThing, 
                      std::string vtkFilename, int binaryFlag = 0 );

   OutFileType _outFileType;///<output XML or classic
   OutFileMode _outFileMode;///<output binary/ascii

   std::string _inFileName;///<input vtk file name
   std::string _outFileName;///<output vtk file name
   vtkXMLFileReadTester* _xmlTester;///<Test if file is XML format   
   vtkDataObject* _dataSet;///<The vtk data.
};
}
#endif// CFD_VTK_FILE_HANDLER_H
