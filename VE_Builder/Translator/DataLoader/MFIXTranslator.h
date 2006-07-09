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
#ifndef _MFIX_TRANSLATOR_H_
#define _MFIX_TRANSLATOR_H_

#include "VE_Builder/Translator/cfdTranslatorToVTK/cfdTranslatorToVTK.h"

namespace VE_Builder
{
class VE_USER_BUILDER_EXPORTS MFIXTranslator: 
   public VE_Builder::cfdTranslatorToVTK
{
public:
   MFIXTranslator();
   virtual ~MFIXTranslator();
   //////////////////////////////////////////////////////
   class VE_USER_BUILDER_EXPORTS MFIXTranslateCbk: public VE_Builder::cfdTranslatorToVTK::TranslateCallback
   {
   public:
      MFIXTranslateCbk(){;}
      virtual ~MFIXTranslateCbk(){;}
      //////////////////////////////////////////////////
      //ouputDataset should be populated              //
      //appropriately by the translate callback.      //
      //////////////////////////////////////////////////
      virtual void Translate(vtkDataSet*& outputDataset,
		                     cfdTranslatorToVTK* toVTK);
   };
   //////////////////////////////////////////////////////
   class VE_USER_BUILDER_EXPORTS MFIXPreTranslateCbk: 
      public VE_Builder::cfdTranslatorToVTK::PreTranslateCallback
   {
   public:
      MFIXPreTranslateCbk(){;}
      virtual ~MFIXPreTranslateCbk(){;}
      void Preprocess(int argc,char** argv,VE_Builder::cfdTranslatorToVTK* toVTK);
   };
protected:
   MFIXPreTranslateCbk cmdParser;
   MFIXTranslateCbk mfixToVTK;
};

}
#endif//_MFIX_TRANSLATOR_H_
