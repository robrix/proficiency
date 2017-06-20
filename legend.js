var Hover = function (element) {
  this.handleEvent = event => {
    let costCentreId = element.id.match(/^legend-(\d+)$/)[1];
    switch (event.type) {
    case 'mouseover':
      document.querySelector("#path-" + costCentreId).classList.add("active");
      break;
    case 'mouseout':
      document.querySelector("#path-" + costCentreId).classList.remove("active");
      break;
    }
  };
  element.addEventListener('mouseover', this);
  element.addEventListener('mouseout', this);
};

function run() {
  for (let li of document.querySelectorAll("#legend li")) {
    li.hover = new Hover(li);
  }
}
