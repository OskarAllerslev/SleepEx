import os
import json
from datetime import date, timedelta
from dotenv import load_dotenv
from garminconnect import Garmin

# Indlæs skjulte variabler fra .env filen
load_dotenv()

def fetch_sleep_data():
    email = os.environ.get("GARMIN_EMAIL")
    password = os.environ.get("GARMIN_PASSWORD")
    
    if not email or not password:
        print("Fejl: Kunne ikke finde GARMIN_EMAIL eller GARMIN_PASSWORD i .env filen.")
        return

    # Vi opretter en skjult mappe til at gemme dine login-cookies
    token_dir = "data-raw/.garmin_tokens"
    os.makedirs(token_dir, exist_ok=True)

    try:
        print("Forbinder til Garmin Connect...")
        client = Garmin(email, password)
        
        # Magien: Vi forsøger at bruge en gemt session først. 
        # Fejler det, logger vi ind rigtigt og gemmer sessionen til næste gang.
        try:
            client.login(token_dir)
            print("Brugte gemt login-session. Smed ingen nye requests til login-serveren!")
        except Exception:
            print("Ingen gyldig session fundet. Logger ind via serveren (dette kan give 429, hvis du er blokeret)...")
            client.login()
            client.garth.save(token_dir)
            print("Nyt login succesfuldt. Session gemt!")
        
        # Sæt tidsrammen (fx de sidste 30 dage)
        slut_dato = date.today()
        start_dato = slut_dato - timedelta(days=30)
        
        print(f"Henter søvndata fra {start_dato} til {slut_dato}...")
        sleep_data = client.get_sleep_data(start_dato.isoformat(), slut_dato.isoformat())
        
        # Gem dataen lokalt
        output_path = "data-raw/garmin_sleep_raw.json"
        with open(output_path, "w", encoding="utf-8") as f:
            json.dump(sleep_data, f, indent=4)
            
        print(f"Succes! Rådata gemt i {output_path}")
        
    except Exception as e:
        print(f"\nDer opstod en fejl: {e}")
        print("Tip: Fik du en 429-fejl? Prøv at skifte til et mobil-hotspot, eller vent 30 minutter.")

if __name__ == "__main__":
    fetch_sleep_data()