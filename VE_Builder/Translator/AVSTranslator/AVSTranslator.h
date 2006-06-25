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
#ifndef _AVS_TRANSLATOR_H_
#define _AVS_TRANSLATOR_H_


#include "VE_Builder/Translator/cfdTranslatorToVTK/cfdTranslatorToVTK.h"

namespace VE_Builder{
class /*VE_BUILDER_EXPORTS*/ AVSTranslator: 
   public VE_Builder::cfdTranslatorToVTK{

public:
   AVSTranslator();
   virtual ~AVSTranslator();
 
   class AVSTranslateCbk: public VE_Builder::cfdTranslatorToVTK::TranslateCallback{
   public:
      AVSTranslateCbk(){};
      virtual ~AVSTranslateCbk(){};
      //////////////////////////////////////////////////
      //ouputDataset should be populated              //
      //appropriately by the translate callback.      //
      //////////////////////////////////////////////////
      virtual void Translate(vtkDataSet*& outputDataset,
		                     cfdTranslatorToVTK* toVTK);
   protected:
   };
   class AVSPreTranslateCbk: public VE_Builder::cfdTranslatorToVTK::PreTranslateCallback{
   public:
      AVSPreTranslateCbk(){};
      virtual ~AVSPreTranslateCbk(){};
      void Preprocess(int argc,char** argv,VE_Builder::cfdTranslatorToVTK* toVTK);
   protected:
   };
protected:
   AVSPreTranslateCbk _cmdParser;
   AVSTranslateCbk _AVSToVTK;
};

}
#endif//_AVS_TRANSLATOR_H_
