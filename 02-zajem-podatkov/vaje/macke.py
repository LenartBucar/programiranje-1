import csv
import os
import re

import requests

###############################################################################
# Najprej definirajmo nekaj pomožnih orodij za pridobivanje podatkov s spleta.
###############################################################################

# definirajte URL glavne strani bolhe za oglase z mačkami
cats_frontpage_url = 'http://www.bolha.com/zivali/male-zivali/macke/'
# mapa, v katero bomo shranili podatke
cat_directory = r'E:\Files\Programming\programiranje-1\02-zajem-podatkov\vaje'
# ime datoteke v katero bomo shranili glavno stran
frontpage_filename = 'page.html'
# ime CSV datoteke v katero bomo shranili podatke
csv_filename = 'TODO'


def download_url_to_string(url: str) -> str:
    """Funkcija kot argument sprejme niz in poskusi vrniti vsebino te spletne
    strani kot niz. V primeru, da med izvajanje pride do napake vrne None.
    """
    try:
        # del kode, ki morda sproži napako
        page_content = requests.get(url)
        if page_content.status_code != requests.codes.ok:
            raise requests.HTTPError()
    except requests.HTTPError() as e:
        # koda, ki se izvede pri napaki
        # dovolj je če izpišemo opozorilo in prekinemo izvajanje funkcije
        print(e)
        return ""
    # nadaljujemo s kodo če ni prišlo do napake
    return page_content.text


def save_string_to_file(text, directory, filename):
    """Funkcija zapiše vrednost parametra "text" v novo ustvarjeno datoteko
    locirano v "directory"/"filename", ali povozi obstoječo. V primeru, da je
    niz "directory" prazen datoteko ustvari v trenutni mapi.
    """
    os.makedirs(directory, exist_ok=True)
    path = os.path.join(directory, filename)
    with open(path, 'w', encoding='utf-8') as file_out:
        file_out.write(text)
    return None


# Definirajte funkcijo, ki prenese glavno stran in jo shrani v datoteko.


def save_frontpage(page, directory, filename):
    """Funkcija shrani vsebino spletne strani na naslovu "page" v datoteko
    "directory"/"filename"."""
    save_string_to_file(download_url_to_string(page), directory, filename)


###############################################################################
# Po pridobitvi podatkov jih želimo obdelati.
###############################################################################


def read_file_to_string(directory, filename):
    """Funkcija vrne celotno vsebino datoteke "directory"/"filename" kot niz."""
    with open(f"{os.path.join(directory, filename)}", "r", encoding="utf-8") as f:
        return f.read()


# Definirajte funkcijo, ki sprejme niz, ki predstavlja vsebino spletne strani,
# in ga razdeli na dele, kjer vsak del predstavlja en oglas. To storite s
# pomočjo regularnih izrazov, ki označujejo začetek in konec posameznega
# oglasa. Funkcija naj vrne seznam nizov.


def page_to_ads(page_content):
    """Funkcija poišče posamezne oglase, ki se nahajajo v spletni strani in
    vrne seznam oglasov."""
    
    ptrn = re.compile(r'<li class="EntityList-item EntityList-item--Regular EntityList-item--n\d+  bp-radix__faux-anchor  ".*?</article>\s*</li>', re.DOTALL)
    res = ptrn.findall(page_content)
    print(res)


# Definirajte funkcijo, ki sprejme niz, ki predstavlja oglas, in izlušči
# podatke o imenu, lokaciji, datumu objave in ceni v oglasu.


def get_dict_from_ad_block(block):
    """Funkcija iz niza za posamezen oglasni blok izlušči podatke o imenu, ceni
    in opisu ter vrne slovar, ki vsebuje ustrezne podatke."""
    
    ptrn = re.compile("""
    <li class="EntityList-item EntityList-item--Regular EntityList-item--n\d+  bp-radix__faux-anchor  "  data-href=".*?"   data-options='{"hasCompare":false,"isAdInSavedList":false,"id":(?P<id>\d+)}'  >
                <article class="entity-body cf">

    
                    <h3 class="entity-title"><a name=".*?" class="link" href=".*?">(?P<name>.*?)</a></h3>
        
                    <div class="entity-thumbnail">
                <a class="link" href=".*?">
                    <img
                        class="img entity-thumbnail-img"
                        src="(?P<image>.*?)"
                        data-src=".*?"
                        alt=".*?"
                    />
                    <noscript>
                        <img
                            class="img entity-thumbnail-img"
                            src=".*?"
                            alt=".*?"
                        />
                    </noscript>
                    <div class="entity-thumbnail-preloader">
                        <div class="Preloader Preloader--center">
                            <div class="Preloader-inner"></div>
                        </div>
                    </div>
                </a>
            </div>
        
        
                    <div class="entity-description">

                
                
                                    <div class="entity-description-main">
                                                                                    <span class="entity-description-itemCaption">Lokacija: </span>(?P<loc>.*?)<br />
                                                                        </div>
                
            </div>
        
                    <div class="entity-pub-date">
                <span class="label">Objavljen:</span>
                <time class="date date--full" datetime="(?P<datetime>)" pubdate="pubdate">.*?</time>
            </div>
        
        
                    <div class="entity-notice entity-notice--saved-ad notice-ad-saved  hidden ">
                            </div>
        
        
                    <div class="entity-tools">
                <ul class="tool-items">

                                            <li class="tool-item">
                            <button class="icon-item tool tool--SaveAd js-veza-save_ad " type="button" title="Shrani oglas"><span class="icon icon--action icon--xs icon--star">Shrani oglas</span></button>
                        </li>
                    
                    
                    
                </ul>
            </div>
        
                    <div class="entity-features">
                <ul class="feature-items cf">

                    
                                            <li class="feature-item">
                            <button class="icon-item feature feature--Map js-faux-anchor" type="button" data-href=".*?" title="Ta oglas je umeščen na zemljevidu"><span class="icon icon--action icon--s icon--map">Prikaži na zemljevidu</span></button>
                        </li>
                    
                    
                    
                    
                    
                                    </ul>
            </div>
        
        
                    <div class="entity-prices">
                <ul class="price-items cf">
                                            <li class="price-item">
                            <strong class="price price--hrk">(?P<price>.*?)</strong>
                        </li>
                                    </ul>
            </div>
        
        
        
    
</article>
            </li>
    """, re.DOTALL)
    
    return ptrn.match(block).groupdict()


# Definirajte funkcijo, ki sprejme ime in lokacijo datoteke, ki vsebuje
# besedilo spletne strani, in vrne seznam slovarjev, ki vsebujejo podatke o
# vseh oglasih strani.


def ads_from_file(filename, directory):
    """Funkcija prebere podatke v datoteki "directory"/"filename" in jih
    pretvori (razčleni) v pripadajoč seznam slovarjev za vsak oglas posebej."""
    return [get_dict_from_ad_block(b) for b in page_to_ads(read_file_to_string(directory, filename))]


###############################################################################
# Obdelane podatke želimo sedaj shraniti.
###############################################################################


def write_csv(fieldnames, rows, directory, filename):
    """
    Funkcija v csv datoteko podano s parametroma "directory"/"filename" zapiše
    vrednosti v parametru "rows" pripadajoče ključem podanim v "fieldnames"
    """
    os.makedirs(directory, exist_ok=True)
    path = os.path.join(directory, filename)
    with open(path, 'w', encoding='utf-8') as csv_file:
        writer = csv.DictWriter(csv_file, fieldnames=fieldnames)
        writer.writeheader()
        for row in rows:
            writer.writerow(row)
    return


# Definirajte funkcijo, ki sprejme neprazen seznam slovarjev, ki predstavljajo
# podatke iz oglasa mačke, in zapiše vse podatke v csv datoteko. Imena za
# stolpce [fieldnames] pridobite iz slovarjev.


def write_cat_ads_to_csv(ads, directory, filename):
    """Funkcija vse podatke iz parametra "ads" zapiše v csv datoteko podano s
    parametroma "directory"/"filename". Funkcija predpostavi, da so ključi vseh
    slovarjev parametra ads enaki in je seznam ads neprazen."""
    # Stavek assert preveri da zahteva velja
    # Če drži se program normalno izvaja, drugače pa sproži napako
    # Prednost je v tem, da ga lahko pod določenimi pogoji izklopimo v
    # produkcijskem okolju
    assert ads and (all(j.keys() == ads[0].keys() for j in ads))
    raise NotImplementedError()


# Celoten program poženemo v glavni funkciji

def main(redownload=True, reparse=True):
    """Funkcija izvede celoten del pridobivanja podatkov:
    1. Oglase prenese iz bolhe
    2. Lokalno html datoteko pretvori v lepšo predstavitev podatkov
    3. Podatke shrani v csv datoteko
    """
    # Najprej v lokalno datoteko shranimo glavno stran

    # Iz lokalne (html) datoteke preberemo podatke

    # Podatke preberemo v lepšo obliko (seznam slovarjev)

    # Podatke shranimo v csv datoteko

    # Dodatno: S pomočjo parametrov funkcije main omogoči nadzor, ali se
    # celotna spletna stran ob vsakem zagon prenese (četudi že obstaja)
    # in enako za pretvorbo

    raise NotImplementedError()


if __name__ == '__main__':
    # main()
    # save_frontpage(cats_frontpage_url, cat_directory, frontpage_filename)
    print(ads_from_file(frontpage_filename, cat_directory))
