/*
 * Copyright (c) 2024 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.tool;

import com.ibm.icu.util.ULocale;
import org.jetbrains.annotations.NotNull;

public enum LanguageCode {
    aa("Afar", "Afaraf"),
    ab("Abkhazian", "Аҧсуа"),
    ae("Avestan", "Avesta"),
    af("Afrikaans", "Afrikaans"),
    ak("Akan", "Akan"),
    am("Amharic", "አማርኛ"),
    an("Aragonese", "Aragonés"),
    ar("Arabic", "العربية"),
    av("Avaric", "авар мацӀ; магӀарул мацӀ"),
    ay("Aymara", "aymar aru"),
    az("Azerbaijani", "azərbaycan dili"),
    ba("Bashkir", "башҡорт теле"),
    be("Belarusian", "Беларуская"),
    bi("Bislama", "Bislama"),
    bg("Bulgarian", "български език"),
    //bh("Bihari", "भोजपुरी"),
    bm("Bambara", "bamanankan"),
    bn("Bengali", "বাংলা"),
    bo("Tibetan", "བོད་ཡིག"),
    br("Breton", "brezhoneg"),
    bs("Bosnian", "bosanski jezik"),
    ca("Catalan", "Català"),
    ch("Chamorro", "Chamoru"),
    co("Corsican", "corsu; lingua corsa"),
    cr("Cree", "ᓀᐦᐃᔭᐍᐏᐣ"),
    cs("Czech", "česky; čeština"),
    cu("Church Slavic", "ѩзыкъ словѣньскъ"),
    cv("Chuvash", "чӑваш чӗлхи"),
    cy("Welsh", "Cymraeg"),
    da("Danish", "Dansk"),
    de("German", "Deutsch"),
    dv("Divehi", "ދިވެހި"),
    dz("Dzongkha", "རྫོང་ཁ"),
    ee("Ewe", "Ɛʋɛgbɛ"),
    el("Greek", "Ελληνικά"),
    en("English", "English"),
    eo("Esperanto", "Esperanto"),
    es("Spanish", "Español"),
    et("Estonian", "eesti; eesti keel"),
    eu("Basque", "euskara; euskera"),
    fa("Persian", "فارسی"),
    ff("Fulah", "Fulfulde"),
    fi("Finnish", "suomi; suomen kieli"),
    fj("Fijian", "vosa Vakaviti"),
    fo("Faroese", "Føroyskt"),
    fr("French", "Français"),
    fy("Western Frisian", "Frysk"),
    ga("Irish", "Gaeilge"),
    gd("Scottish Gaelic", "Gàidhlig"),
    gl("Galician", "Galego"),
    gn("Guaraní", "Avañe'ẽ"),
    gu("Gujarati", "ગુજરાતી"),
    gv("Manx", "Gaelg, Gailck"),
    ha("Hausa", "هَوُسَ"),
    he("Hebrew", "עברית"),
    hi("Hindi", "हिन्दी; हिंदी"),
    ho("Hiri Motu", "Hiri Motu"),
    hr("Croatian", "Hrvatski"),
    ht("Haitian", "Kreyòl ayisyen"),
    hu("Hungarian", "Magyar"),
    hy("Armenian", "Հայերեն"),
    hz("Herero", "Otjiherero"),
    ia("Interlingua (International Auxiliary Language Association)", "Interlingua"),
    id("Indonesian", "Bahasa Indonesia"),
    ie("Interlingue", "Interlingue"),
    ig("Igbo", "Igbo"),
    ii("Sichuan Yi", "ꆇꉙ"),
    ik("Inupiaq", "Iñupiaq; Iñupiatun"),
    io("Ido", "Ido"),
    is("Icelandic", "Íslenska"),
    it("Italian", "Italiano"),
    iu("Inuktitut", "ᐃᓄᒃᑎᑐᑦ"),
    ja("Japanese", "Nihongo"),
    ka("Georgian", "ქართული"),
    kg("Kongo", "KiKongo"),
    ki("Kikuyu", "Gĩkũyũ"),
    kj("Kwanyama", "Kuanyama"),
    ku("Kurdish", "كوردی"),
    kk("Kazakh", "Қазақ тілі"),
    kl("Kalaallisut", "kalaallisut; kalaallit oqaasii"),
    km("Khmer", "ភាសាខ្មែរ"),
    kn("Kannada", "ಕನ್ನಡ"),
    ko("Korean", "Kanuri"),
    ks("Kashmiri", "коми кыв"),
    kw("Cornish", "Kernewek"),
    ky("Kirghiz", "кыргыз тили"),
    la("Latin", "Latine; lingua latina"),
    lb("Luxembourgish", "Lëtzebuergesch"),
    lg("Ganda", "Luganda"),
    li("Limburgish", "Limburgs"),
    ln("Lingala", "Lingála"),
    lo("Lao", "ພາສາລາວ"),
    lt("Lithuanian", "Lietuvių kalba"),
    lu("Luba-Katanga", "Latviešu valoda"),
    mg("Malagasy", "Malagasy fiteny"),
    mh("Marshallese", "Kajin M̧ajeļ"),
    mi("Māori", "Te reo Māori"),
    mk("Macedonian", "македонски јазик"),
    ml("Malayalam", "മലയാളം"),
    mn("Mongolian", "Монгол"),
    mr("Marathi", "मराठी"),
    ms("Malay", "بهاس ملايو"),
    mt("Maltese", "Malti"),
    my("Burmese", "ဗမာစာ"),
    na("Nauru", "Ekakairũ Naoero"),
    nb("Norwegian Bokmål", "Norsk bokmål"),
    nd("North Ndebele", "isiNdebele"),
    ne("Nepali", "नेपाली"),
    ng("Ndonga", "Owambo"),
    nl("Dutch", "Nederlands"),
    nn("Norwegian Nynorsk", "Norsk nynorsk"),
    no("Norwegian", "Norsk"),
    nr("South Ndebele", "isiNdebele"),
    nv("Navajo", "Diné bizaad; Dinékʼehǰí"),
    ny("Chichewa", "chiCheŵa; chinyanja"),
    oc("Occitan", "Occitan"),
    oj("Ojibwa", "ᐊᓂᔑᓈᐯᒧᐎᓐ"),
    om("Oromo", "Afaan Oromoo"),
    or("Oriya", "ଓଡ଼ିଆ"),
    os("Ossetian", "Ирон æвзаг"),
    pa("Panjabi", "पाऴि"),
    pl("Polish", "Polski"),
    ps("Pashto", "پښتو"),
    pt("Portuguese", "Português"),
    qu("Quechua", "Runa Simi; Kichwa"),
    rm("Raeto-Romance", "rumantsch grischun"),
    rn("Kirundi", "kiRundi"),
    ro("Romanian", "Română"),
    ru("Russian", "русский язык"),
    rw("Kinyarwanda", "Ikinyarwanda"),
    sa("Sanskrit", "संस्कृतम्"),
    sc("Sardinian", "sardu"),
    sd("Sindhi", "Davvisámegiella"),
    sg("Sango", "yângâ tî sängö"),
    si("Sinhala", "සිංහල"),
    sk("Slovak", "Slovenčina"),
    sl("Slovenian", "Slovenščina"),
    sm("Samoan", "gagana fa'a Samoa"),
    sn("Shona", "chiShona"),
    so("Somali", "Soomaaliga; af Soomaali"),
    sq("Albanian", "Shqip"),
    sr("Serbian", "српски језик"),
    ss("Swati", "SiSwati"),
    st("Southern Sotho", "Sesotho"),
    su("Sundanese", "Basa Sunda"),
    sv("Swedish", "svenska"),
    sw("Swahili", "Kiswahili"),
    ta("Tamil", "தமிழ்"),
    te("Telugu", "తెలుగు"),
    tg("Tajik", "ไทย"),
    ti("Tigrinya", "ትግርኛ"),
    tk("Turkmen", "Türkmen; Түркмен"),
    //tl("Tagalog", "Tagalog"),
    tn("Tswana", "Setswana"),
    to("Tonga", "faka Tonga"),
    tr("Turkish", "Türkçe"),
    ts("Tsonga", "Xitsonga"),
    tt("Tatar", "Twi"),
    ty("Tahitian", "Reo Mā`ohi"),
    ug("Uighur", "Українська"),
    ur("Urdu", "اردو"),
    uk("Ukrainian", "Ukraïna"),
    uz("Uzbek", "Tshivenḓa"),
    vi("Vietnamese", "Tiếng Việt"),
    vo("Volapük", "Volapük"),
    wa("Walloon", "Walon"),
    wo("Wolof", "Wollof"),
    xh("Xhosa", "isiXhosa"),
    yi("Yiddish", "ייִדיש"),
    yo("Yoruba", "Yorùbá"),
    za("Zhuang", "Saɯ cueŋƅ; Saw cuengh"),
    zh("Chinese", "中文 (Zhōngwén), 汉语, 漢語"),
    zu("Zulu", "isiZulu");

    private final String readableName;
    private final String nativeName;
    LanguageCode(String readableName, String nativeName) {
        this.readableName = readableName;
        this.nativeName = nativeName;
    }
    public String readableName() { return readableName;}
    public String nativeName() { return nativeName;}
    public @NotNull String getISO3Language() throws IllegalArgumentException {
        ULocale locale = new ULocale(this.name());
        var isocode = locale.getISO3Language();
        if (isocode.isEmpty())
            throw new IllegalArgumentException("Language code '" + this.name() + "' is empty");
        else return isocode;
    }

    public static LanguageCode fromNativeName(@NotNull String nativeName) throws IllegalArgumentException {
        for (var item: LanguageCode.values()) {
            if (item.nativeName.equals(nativeName))
                return item;
        }
        throw new IllegalArgumentException("Language code '" + nativeName + "' not found");
    }
}
