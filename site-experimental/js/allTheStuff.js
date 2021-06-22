var THINGS;

$.getJSON("data/pubs.json", function(data) {
  THINGS = data;
})
.done(function() {
  addTheThings();
  // buildAlbums(pics, albums);
  // showListOfAlbums();
  // showAlbum(0);
  // console.log(THINGS);
});
console.log(THINGS);

const tileImage = "images/tile.png";
const tileSize = "150px";

function addTheThings() {
  for (i in THINGS) {
    var thing = THINGS[i];
    var thingId = thing.Key;
    var thingHref = thingId + "Href";
    var thingImg = thingId + "Img";
    var opacity = Math.random();
    $( "#thethings" ).append(`<div id="${ thingId }">
                                <img id="${ thingImg }" src="${ tileImage }">
                                <p><a id="${ thingHref}">${ thing.DOI }</a></p>
                              </div>`);

    $(`#${ thingId }`).css({"flex": `1 0 ${ tileSize }`});
    $(`#${ thingId }`).attr("class", "testing");

    $(`#${ thingImg }`).css({"opacity": `${ opacity }`, "height": "100%", "width": "100%"});

    $(`#${ thingHref }`).attr({"href": `https://dx.doi.org/${ thing.DOI }`});
  }
}
