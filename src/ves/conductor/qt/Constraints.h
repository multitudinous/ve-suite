/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/
#pragma once

#define QT_NO_KEYWORDS

#include <QtGui/QDialog>

#include <propertystore/PropertySetPtr.h>

#include <ves/xplorer/Logging.h>
#include <switchwire/ScopedConnectionList.h>

#include <vector>
#include <string>

// Forward declarations
namespace Ui
{
class Constraints;
}
/*!\file Constraints.h
 * Constraints Qt Window
 * \class ves::conductor::Constraints
 * This class manages the Constraints Qt window.
 * \namespace ves::conductor
 * UI Namespace
 */

namespace ves
{
namespace conductor
{
//class PropertyBrowser;

class Constraints : public QDialog
{
    Q_OBJECT
public:
    ///Constructor
    Constraints( QWidget* parent = 0 );
    \
    ///Destructor
    ~Constraints();

protected:
    void changeEvent( QEvent* e );

    /** Updates the feature choices in the dropdown box.
      * Important side effects (that may not be obvious): Clears the list
      * associated with the dropdown box, which emits a signal that its current
      * selection has been changed to -1 (null selection). Adds items to the list
      * based on database and (possible) presence of an unsaved PropertySet; the
      * dropdown auto-selects the first item (index 0) when these items are added,
      * causing it to emit its selection changed signal. Note that at this point
      * we have still not explicitly changed the selection -- this stuff happens
      * automatically and causes the construction and destruction of PropertySets
      * each time.
      *
      * It is highly reccomended that any internal methods that call this
      * explicitly set the current index afterwards, since the above behavior is
      * internal to Qt and might change.
      **/
    void UpdateFeatureIDSelectorChoices();

protected Q_SLOTS:
    // For info on Automatic connection of signals and slots, see
    // http://doc.trolltech.com/4.6/designer-using-a-ui-file.html#automatic-connections
    void on_WritePropertiesButton_clicked(); // Automatic connection
    void on_RefreshPropertiesButton_clicked(); // Automatic connection
    void on_NewFeatureButton_clicked(); // Automatic connection
    void on_DeleteFeatureButton_clicked(); // Automatic connection
    void on_FeaturesList_currentTextChanged( const QString& currentText ); // Automatic connection
    void on_FeatureIDSelector_currentIndexChanged( int index );  // Automatic connection


private:
    void ResyncFromDatabaseSlot();
    Ui::Constraints* m_ui;
    ///The property browser widget for all constraints
    //PropertyBrowser* mFeatureBrowser;
    ///The current property set being worked with on the constraints pane
    propertystore::PropertySetPtr mTempSet;
    ///A vector of ids for something
    std::vector< std::string > m_ids;
    ///Logger reference
    Poco::Logger& m_logger;
    ///Actual stream for this class
    ves::xplorer::LogStreamPtr m_logStream;
    ///Required to connect to EventManagered signals
    switchwire::ScopedConnectionList m_connections;
};

} // namespace conductor
} // namespace ves
