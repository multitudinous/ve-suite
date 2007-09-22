VE-Suite Data Viewer

readme.txt
Last Updated: 13 July 2005



INSTALLATION

Prior to beginning installation, read through all of readme.txt, as it 
includes information regarding server and browser requirements. Additionally, 
make sure that your system administrator has created or specified to you a 
database for use by the VE-Suite Data Viewer.

Depending on your server configuration, installation should be complete in 
15 minutes or less, barring any unforseen errors.

During the installation, the web application will primarily set up database 
tables specifically for use by the VE-Suite Data Viewer. Please make sure you 
have your MySQL database and web-side usernames and passwords available.

To install the VE-Suite Data Viewer, use the following instructions.

1. Unzip data_viewer.zip to the desired location on your webserver. This
   location should be where you will access the VE-Suite Data Viewer in your
   browser.

	Example:	File Path: \home\users\data_viewer
			Web Address: http://www.yoursite.com/data_viewer/

2. Open a new shell and go to the file path where the VE-Suite Data Viewer
   files were copied.

3. Run the shell script "permissions.sh" -- This will ensure that all
   files are set to the proper permissions as used by the VE-Suite Data Viewer.

4. In your browser, navigate to the web address at which the VE-Suite Data 
   Viewer _installation folder is located.

	Example:	http://www.yoursite.com/data_viewer/_installation/

   This will setup database tables, upload a default component diagram, and
   add new users.



SERVER REQUIREMENTS

The VE-Suite Data Viewer was developed for the following server specifications:

	* Apache 2.0
	* PHP 4.3.9
	* MySQL 4.0.13
	* SSL capable

In order for this web application to function properly, we suggest that your 
server be upgraded to these specifications or beyond. For security purposes, 
your server must be SSL capable and enabled.
 


BROWSER REQUIREMENTS

This web application was developed using the Mozilla Firefox browser. 
Although not a requirement, for aesthetic purposes, we suggest that Firefox 
1.0.2 or greater be used when using this website. This web application will 
function properly on Mozilla and Internet Explorer browsers.

	* This application requires that your browser be set to recieve 
	  cookies for authentication purposes.
	* If you have a pop-up blocker installed on your computer, or if a 
	  pop-up blocker feature is included in your browser, you must 
	  either disable it or put this site on the allowed list to use the 
	  chat and Virtual Viewer features.
	* To use many of the available features, JavaScript must be enabled. 
	  JavaScript is enabled by default on most browsers; if you have 
	  previously disabled JavaScript, you will want to enable it to take 
	  advantage of all features.
	* SSL is used to create a secure connection between your computer 
	  and this web application. SSL must be enabled to help secure your 
	  data.

This web application has also been designed for minimum screen resolution of 
1024 by 768 pixels. To be able to view this site properly, we suggest you 
verify that your screen resolution is set at or above this setting.



UNINSTALLING VE-SUITE DATA VIEWER

To uninstall VE-Suite Data Viewer, login to the data viewer and go to the
Administrative Options section. There is an option titled "Uninstall VE-Suite
Data Viewer." You will need your MySQL database username and password, as well
as your own administrative username and password.

The uninstaller only drops tables from the MySQL database. It is necessary that
you delete the VE-Suite Data Viewer folder from your server once you have
completed the uninstall.

To reinstall VE-Suite Data Viewer, all files and tables must be deleted. In
other words, you must do a "clean install."