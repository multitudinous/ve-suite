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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef PLOT3DREADER_H
#define PLOT3DREADER_H

class vtkStructuredGridWriter;
class vtkStructuredGrid;
class vtkPLOT3DReader;
class vtkUnstructuredGridWriter;
class vtkUnstructuredGrid;
class vtkAppendFilter;

#include "VE_Builder/Translator/cfdTranslatorToVTK/cfdTranslatorToVTK.h"

namespace VE_Builder
{
class  VE_USER_BUILDER_EXPORTS plot3dReader: public VE_Builder::cfdTranslatorToVTK
{
public:
   plot3dReader( void );
   ~plot3dReader( void );
   plot3dReader( plot3dReader * );
   ///Display help for the Plot3D translator
   virtual void DisplayHelp( void );
   //////////////////////////////////////////////////////
   class VE_USER_BUILDER_EXPORTS Plot3DTranslateCbk: public VE_Builder::cfdTranslatorToVTK::TranslateCallback
   {
   public:
      Plot3DTranslateCbk(){;}
      virtual ~Plot3DTranslateCbk(){;}
      //////////////////////////////////////////////////
      //ouputDataset should be populated              //
      //appropriately by the translate callback.      //
      //////////////////////////////////////////////////
      virtual void Translate(vtkDataObject*& outputDataset,
                             cfdTranslatorToVTK* toVTK);
      ///This creates additional scalars from vector components
      ///\param outputDataset Dataset to be used and modified
   private:
      vtkStructuredGridWriter    *writer;      
      vtkPLOT3DReader            *reader;
      vtkStructuredGrid          **grids;
      vtkUnstructuredGrid        *unsgrid;
      vtkUnstructuredGridWriter  *unswriter;
      vtkAppendFilter            *filter;      
      int   answer;   
      int   numGrids;
      char  plot3dGeomFileName[100];
      char  plot3dDataFileName[100];
      int   numOfSurfaceGrids;
   };
   //////////////////////////////////////////////////////
   class VE_USER_BUILDER_EXPORTS Plot3DPreTranslateCbk: 
      public VE_Builder::cfdTranslatorToVTK::PreTranslateCallback
   {
   public:
      Plot3DPreTranslateCbk(){;}
      virtual ~Plot3DPreTranslateCbk(){;}
      void Preprocess(int argc,char** argv,VE_Builder::cfdTranslatorToVTK* toVTK);
      std::string GetIBlankFlag( void );
      std::string GetNumberOfDimensions( void );
      std::string GetMultigridFlag( void );
      std::string GetXYZFilename( void );
      std::string GetQFilename( void );
      std::string GetByteFlag( void );
      std::string GetBinaryFlag( void );
      std::string GetByteCountFlag( void );
      std::string GetForceReadFlag( void );
   private:
      std::string numberOfDimensions;
      std::string iblankFlag;
      std::string multiGridFlag;
      std::string xyzFilename;
      std::string qFilename;
      std::string byteFlag;
      std::string binaryFlag;
      std::string byteCountFlag;
      std::string forceReadFlag;
   };
   
   //void writeParticlePolyData( void );
   //void readPPLOT1( void );
   void GetFileNames( void );
   //void readParticleParamFile( void );
   vtkUnstructuredGrid  *GetUnsGrid( void );
   vtkUnstructuredGrid *MakeVTKSurfaceFromGeomFiles( void );

   //typedef std::vector< Particle * > Particles;
   //Particles particles;
   //int nsl;
   //int nps;

   char  *plot3dSurfaceFileName[100];

protected:
   Plot3DPreTranslateCbk cmdParser;
   Plot3DTranslateCbk plot3dToVTK;
};
}
#endif
