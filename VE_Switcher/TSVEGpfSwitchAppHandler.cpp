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
