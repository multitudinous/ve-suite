/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 * File:          $RCSfile: filename,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_VTK_FILE_HANDLER_H
#define CFD_VTK_FILE_HANDLER_H
class vtkXMLFileReadTester;
class vtkDataSet;

#include "VE_Installer/include/VEConfig.h"
#include <string>

namespace VE_Util
{
class VE_UTIL_EXPORTS cfdVTKFileHandler
{
public:
   cfdVTKFileHandler();
   cfdVTKFileHandler(const cfdVTKFileHandler& fh);
   virtual ~cfdVTKFileHandler();

   enum OutFileType{CFD_XML,VTK_CLASSIC};
   enum OutFileMode{CFD_ASCII=0,CFD_BINARY};

   void SetInputFileName(std::string inFile);
   void SetOutputFileName(std::string oFile);
   void SetVTKOutFileType(OutFileType type);
   void SetOutFileWriteMode(OutFileMode mode);

   vtkDataSet* GetDataSetFromFile(std::string vtkFileName);
   bool WriteDataSet(vtkDataSet* dataSet,std::string outFileName);

   cfdVTKFileHandler& operator=(const cfdVTKFileHandler& fh);
protected:
   void _getXMLUGrid();
   void _getXMLSGrid();
   void _getXMLRGrid();
   void  _getXMLPolyData();
   ///Reader function to open an vtkImageData file
   void GetXMLImageData( void );
   void _readClassicVTKFile();
   void _writeClassicVTKFile( vtkDataSet * vtkThing, 
                      std::string vtkFilename, int binaryFlag = 0 );

   OutFileType _outFileType;
   OutFileMode _outFileMode;

   std::string _inFileName;
   std::string _outFileName;
   vtkXMLFileReadTester* _xmlTester;   
   vtkDataSet* _dataSet;
};
}
#endif// CFD_VTK_FILE_HANDLER_H
