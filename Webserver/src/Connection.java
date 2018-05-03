/**
 * This is the separate thread that services each
 * incoming echo client request.
 *
 * @author Greg Gagne
 *
 * Used for HW3 Networks
 */

import java.net.*;
import java.io.*;
import java.net.URL;
import java.util.Calendar;
import java.nio.charset.StandardCharsets;


public class Connection implements Runnable
{
	public static final int BUFFER_SIZE = 2048;
	public static final int PORT = 8080;


	private Socket client;
	private Configuration config;

	public Connection(Socket client, Configuration config) {
		this.client = client;
		this.config = config;
	}

    /**
     * This method runs in a separate thread.
     */
	public void run() {
		try {

			String[] contentType = {"text/html", "image/gif", "image/jpeg", "image/png", "text/plain", "video/mp4"};

			try{

				BufferedReader is = new BufferedReader(new InputStreamReader(client.getInputStream()));
				OutputStream os = new BufferedOutputStream(client.getOutputStream());

				//Set up the request, parsing:
				String innLine = is.readLine();
				String[] part = innLine.split(" ");
				String logCommand = innLine; //for logging 
				String pastPath = part[1];
				String req;
				File requestedFile = null;
				String fileType;
				String content = " ";
				String contentSize;
				String code;
				long contentSizeLong = 0;
				String date;
				String server;
				String conStatus;
				String beam;//request
				String log;

				if (innLine.substring(0,3).equals("GET") ){
					String testFile = pastPath.replaceFirst("/","");

					testFile = URLDecoder.decode(testFile, StandardCharsets.UTF_8.toString());

					if(pastPath.equals("/")) {
						testFile = config.getDefaultDocument();
					}

					if(new File(config.getDocumentRoot() + testFile).isFile()){
						req = "HTTP/1.1 200 OK \r\n";
						code = "200";
						requestedFile = new File(config.getDocumentRoot() + testFile);

						fileType = testFile.substring(testFile.lastIndexOf("."));

						//make content the fileType
						if(fileType.equals(".html")){
							content = contentType[0];
						}
						else if(fileType.equals(".gif")){
							content = contentType[1];
						}
						else if(fileType.equals(".jpeg")){
							content = contentType[2];
						}
						else if(fileType.equals(".png")){
							content = contentType[3];
						}
						else if(fileType.equals(".txt")){
							content = contentType[4];
						}
						else if(fileType.equals(".mp4")){
							content = contentType[5];
						}

						contentSizeLong = requestedFile.length();
						
					}
					else if(testFile == config.getDefaultDocument()){
						req = "HTTP/1.1 200 OK \r\n";
						code = "200";
						requestedFile = new File(config.getDefaultDocument());
						content = contentType[0];
						contentSizeLong = requestedFile.length();
					}
					else{
						req = "HTTP/1.1 404 NOT FOUND \r\n";
						code = "404";
						requestedFile = new File(config.getDefaultDocument());
						content = contentType[0];
						contentSizeLong = requestedFile.length();
						//what should be returned to the browser if there is a 404 not FOUND
					}
				}
				else{
					//tecknically a 405 error
					client.close();
					return;
				}

				// System.out.println("What is going on with the requestFile: " + requestedFile);

				/*write the HEADERS
				* HTTP/1.1 <200 OK/ 404 NOT fund>
				* Date: <todays date>
				* Server Name: <Server Name (from config)>
				* Content-Type: <contentType[x]>
				* Content-Lenght: contentType[x].length
				* Connection: closed
				*/
				//req
				date = "Date: "+ Calendar.getInstance().getTime() + "\r\n";
				server = "Server: " + config.getServerName() + "\r\n";
				content = "Content-Type: " + content + "\r\n";
				contentSize = "Content-Size: " + String.valueOf(contentSizeLong) + "\r\n";
				conStatus = "Connection: closed\r\n\r\n";

				log = client.getInetAddress().toString() + " [" + Calendar.getInstance().getTime() + "] " + logCommand + " " + code + " " + String.valueOf(contentSizeLong);

				beam = req + date + server + content + contentSize + conStatus;
				System.out.println(beam);

				os.write(beam.getBytes());
				os.flush();

				File logFile = new File(config.getLogFile());
				BufferedWriter writer = new BufferedWriter(new FileWriter(logFile, true));
				writer.write(log+"\n");
				writer.flush();
				writer.close();

				//System.out.println("Yoyoy we are past the flush");

				FileInputStream fileIn = new FileInputStream(requestedFile);
				byte[] buffer = new byte[BUFFER_SIZE];
				int numBytes;

				while((numBytes = fileIn.read(buffer)) != -1){
					os.write(buffer, 0, numBytes);
				}
				fileIn.close();

//close
				os.close();
				is.close();
				client.close();

			}//end try
			catch (Exception e){
				System.err.println(e);
			}
			finally{
				client.close();
			}
		}//end try
		catch (java.io.IOException ioe) {
			System.err.println(ioe);
		}
	}
}//end of connection
