package abs.fli.java;

import org.openqa.selenium.WebDriver;
import org.openqa.selenium.firefox.FirefoxDriver;

import WebDriver.FireFox.WebDriver_i;
import abs.backend.java.fli.ABSForeignObject;
import abs.backend.java.lib.types.ABSString;
import abs.backend.java.lib.types.ABSUnit;

public class WebDriverImpl extends ABSForeignObject implements WebDriver_i {
    
    private final WebDriver driver = new FirefoxDriver();
    private final PrimitiveUtil putil = new PrimitiveUtil();
    
    public ABSUnit getPage(ABSString s) {
        driver.get(putil.convert(s));
        return ABSUnit.UNIT;
    }   
    
    public ABSString getCurrentUrl() { 
        return putil.convert(driver.getCurrentUrl()); 
    }
}
