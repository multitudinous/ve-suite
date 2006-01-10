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
 * File:          $RCSfile: TSVEGpfSwitchAppHandler.cpp,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include<TSVEGpfSwitchAppHandler.h>


   pfBinaryFiles::pfBinaryFiles( void )
   {
   }


   pfAppHandle::pfAppHandle( pfGroup *temp)
   {
      this->rootNode = temp;
      this->thisApp = new pfDCS;
   }   


   void pfAppHandle::setIconScale(float scale)
   {
      iconScale = scale;
   }


   void pfAppHandle::addToGraph(pfNode *model)
   {
      this->thisApp->addChild(model);
   }


   void pfAppHandle::selectApp( float percent )
   {
      float scalefactor = (1.0 -  iconScale)*percent;
      this->thisApp->setScale( iconScale + scalefactor );
      this->thisApp->setTrans(0.0f, 0.0f, (1-percent)*4);
   }


   void pfAppHandle::deSelectApp( float percent )
   {
      float scalefactor = (1.0 -  iconScale)*percent;
      this->thisApp->setScale( 1 - scalefactor );
      this->thisApp->setTrans(0.0f, 0.0f, (percent)*4);
   }


   void pfAppHandle::setPos(float z)
   {
      this->thisApp->setTrans(0.0f, 0.0f, z);
   }


   void pfAppHandle::bringAppIn( float percent )
   {
      this->thisApp->setTrans(0.0f, 0.0f, 4 + (1-percent)*5);
   }

   void pfAppHandle::pushAppOut( float percent )
   {
      this->thisApp->setTrans(0.0f, 0.0f, 4-(percent*5));
   }

   void pfAppHandle::addDCSToRoot( void )
   {
      this->thisApp->setTrans(0.0f, 0.0f, 4.0f);
      this->thisApp->setScale(iconScale);
      this->rootNode->addChild( this->thisApp );
   }


   void pfAppHandle::removeDCSFromRoot( void )
   {
      this->rootNode->removeChild( this->thisApp );
   }


   void pfAppHandle::setPosAllAxis(float x, float y, float z)
   {
      this->thisApp->setTrans( x, y, z );
   }
