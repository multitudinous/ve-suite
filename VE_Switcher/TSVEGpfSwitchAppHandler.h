/**************************TSVEG Switcher App Handler***************************/
#ifndef TSVEG_PF_SWITCH_APP_HANDLER_H_
#define TSVEG_PF_SWITCH_APP_HANDLER_H_


#include <Performer/pf/pfDCS.h>
#include <Performer/pf/pfSwitch.h>
#include <Performer/pf/pfNode.h>
#include <Performer/pf/pfGroup.h>
#include "cfdNavigate.h"

class pfBinaryFiles
{
public:

   //Constructor sets up a character array
   pfBinaryFiles( void );

   char binFiles[100];
   float pfBinScale;
};


class pfAppHandle
{
public:

   //Contructor grabs the rootnode and declares a new pfDCS
   pfAppHandle( pfGroup *temp );
   
   //Lets the user set the icon scale of the app
   void setIconScale(float);

   //Adds the model to the pfDCS
   void addToGraph(pfNode *);

   //Lets the app add itself to the rootnode
   void addDCSToRoot( void );

   //Lets the app take itself off of the rootnode
   void removeDCSFromRoot( void );

   //Steps into the pfDCS
   void selectApp( float );

   //Steps out of the pfDCS
   void deSelectApp( float );

   //Controls z-axis translation
   void setPos(float);

   //Translates the next app into the room  
   void bringAppIn( float );

   //Translates the previous app out of the room
   void pushAppOut( float );

   //Controls the apps position 
   void setPosAllAxis(float, float, float);

private:
   pfGroup *rootNode;
   pfDCS* thisApp;
   float iconScale;

};
#endif

