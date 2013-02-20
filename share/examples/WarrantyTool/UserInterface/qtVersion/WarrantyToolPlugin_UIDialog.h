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
#ifndef WarrantyToolPlugin_UIDIALOG_H
#define WarrantyToolPlugin_UIDIALOG_H

#include <QtGui/QWidget>
#include <QtGui/QComboBox>
#include <QtCore/QStringList>
#include <QtGui/QListWidgetItem>
#include <QtGui/QFileDialog>
#include <QtGui/QTreeWidgetItem>

#include <string>
#include <vector>
#include <map>

#include <osg/Node>

#include <switchwire/ScopedConnectionList.h>

#include <ves/util/SimpleDataTypeSignalSignatures.h>

namespace Ui {
    class WarrantyToolPlugin_UIDialog;
}

class WarrantyToolPlugin_UIDialog : public QWidget
{
    Q_OBJECT

public:
    explicit WarrantyToolPlugin_UIDialog(QWidget *parent = 0);
    ~WarrantyToolPlugin_UIDialog();
    void ParseDataFile( const std::string& csvFilename );
    void ParseDataBase( const std::string& csvFilename );
    void OnDataLoad( std::string const& fileName );

protected:
    void changeEvent(QEvent *e);
    void QueryUserDefinedAndHighlightParts( const std::string& queryString );

protected slots:
    /// Toggles succeeding logic blocks on and off depending on value of
    /// selection. (Not autoconnected)
    void m_logicOperatorS_currentIndexChanged ( QString const& text );
    /// Submits user-entered query. (Autoconnected)
    void on_m_queryTextCommandCtrl_returnPressed(  );
    void on_m_applyButton_clicked( );
    void m_variableChoiceS_changed( QString const& text );
    void m_variableLogicOperatorS_changed( QString const& text );
    ///Autoconnect the state change call for the mouse selection toggle
    void on_m_mouseSelection_clicked( bool checked );
    ///Autoconnect the state change call for the toggle unselected
    void on_m_toggleUnselected_clicked( bool checked );
    ///Autoconnect the clear button
    void on_m_clear_clicked();
    ///Slot called when user tries to close one of this page's sub-tabs
    void on_m_tabWidget_tabCloseRequested ( int index );

    /// Called whenever a checkbox is toggled in the "Text Display Selection"
    /// widget. (Autoconnected)
    void on_m_displayTextChkList_itemClicked( QListWidgetItem* item );
    /// Called when Create Table checkbox is toggled. (Autoconnected)
    void on_m_createTableFromQuery_toggled();
    void InputTextChanged ( const QString& text );
    ///Autoconnect the data load button
    void on_m_dataLoadButton_clicked();
    void on_m_fileBrowseButton_clicked();
    void onFileSelected( const QString& filePath );
    void onFileCancelled();
    void QueryItemChanged( QTreeWidgetItem* current, QTreeWidgetItem* previous );

private:
    void StripCharacters( std::string& data, const std::string& character );
    const std::string GetTextFromChoice( QComboBox* variable,
                                         QComboBox* logicOperator,
                                         QLineEdit* textInput );
    const std::string GetTextFromLogicOperator( QComboBox* logicOperator );
    void SubmitQueryCommand();
    void UpdateQueryDisplay();
    void PartSelected( const std::string& partNumber );

    Ui::WarrantyToolPlugin_UIDialog *ui;

    std::vector< std::string > mPartNumberList;
    ///PArt numbers loaded from the csv files
    std::vector< std::string > mLoadedPartNumbers;
    ///Description of part numbers loaded from csv files
    std::vector< std::string > mPartNumberDescriptions;
    //wxComboBox* mPartListCMB; // now lives in .ui file and is called m_partListCombo
    //wxArrayString m_partNumberStrings;
    QStringList m_partNumberStrings;
    //wxArrayString m_columnStrings;
    QStringList m_columnStrings;
    ///The number of tables created by the user
    size_t m_tableCounter;
    ///List of tables created by the user
    std::vector< std::string > m_tableList;
    std::string m_filename;

    QFileDialog* m_fileDialog;
    ///The connect signal for toggle options
    ves::util::BoolSignal_type m_connectToggleUnselectedSignal;
    ///The connect signal for toggle options
    ves::util::BoolSignal_type m_connectMouseSelectionSignal;
    ///The connect signal for toggle options
    ves::util::VoidSignal_type m_clearSignal;
    ///Highlight parts via part number
    ves::util::StringSignal_type m_highlightPartSignal;
    ///Submit custom query
    ves::util::StringSignal_type m_querySignal;

    switchwire::Event< void(std::vector<std::string>&) > m_highlightPartsSignal;

    /// Required to connect to switchwire signals
    switchwire::ScopedConnectionList m_connections;
};

#endif // WarrantyToolPlugin_UIDIALOG_H
