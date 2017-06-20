var Hover = function (element, target) {
  this.handleEvent = event => {
    switch (event.type) {
    case 'mouseover':
      target.classList.add("active");
      break;
    case 'mouseout':
      target.classList.remove("active");
      break;
    }
  };
  element.addEventListener('mouseover', this);
  element.addEventListener('mouseout', this);
};

function run() {
  for (let li of document.querySelectorAll("#legend li")) {
    let costCentreId = li.getAttribute("data-id");
    let path = document.querySelector("#path-" + costCentreId);
    li.hover = new Hover(li, path);
  }
  for (let path of document.querySelectorAll("#graph path")) {
    let costCentreId = path.getAttribute("data-id");
    let li = document.querySelector("#legend-" + costCentreId);
    path.hover = new Hover(path, li);
  }
}
